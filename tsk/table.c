/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             table.dc1

Program:       TABLE

Purpose:       GIPSY Table maintenance program

Category:      TABLES, UTILITY, HEADER

File:          table.c

Author:        M.G.R. Vogelaar

Notes:         A GIPSY table is a collection of columns of type
               INT (integer numbers), REAL (floating point numbers),
               DBLE (double precision numbers), LOG (booleans) or
               CHARn (strings of length n which can contain spaces).
               Table entries are stored in the descriptor of a set
               and are bound to a subset level. Therefore a column
               can be identified by:
               1) Table name
               2) Column name
               3) Set name
               4) Subset level
               The program first asks set and subset and then generates
               a list with all the columns it could find. This list
               is called the 'table directory'. Each table in this
               directory has a unique 'table number' and each column
               in a table also has a unique 'column number'. All
               input of columns can be given either with these numbers
               or by the name of the table and column. The keywords
               containing TABCOL accept names or numbers or a mix
               of both. The first entry must always be a table (one name
               or one number) and the others must be columns. If a
               table specification is not unique, set and subset are
               asked with COLSET=

               Examples:

               Set AURORA has axis RA-DEC-FREQ.
               INSET=AURORA TOP=N   produces the table directory:

               ============================================================
               tab col Table    Column   Type     Units  Size   Level   set
               ============================================================
               1   1   OBSFREQ  XX       INT   UNITS  255    (* * *) AURORA
               1   2   OBSFREQ  YY       INT   UNITS  255    (* * *) AURORA
               2   1   OBSINTF  BASELINE INT   ?      19     (* * *) AURORA
               3   1   XBLPOS   xpos     REAL  WU     5      (* * 2) AURORA
               4   1   XBLPOS   xpos     REAL  WU     5      (* * 3) AURORA

               TABCOL=1              : select all columns in table 1
               TABCOL=1 1 2          : select column 1 and 2 of table 1
               TABCOL=OBSFREQ        : select all columns in table 1 (OBSFREQ)
               TABCOL=OBSFREQ 1 2    : select column 1 and 2 in table OBSFREQ

               All specifications return  columns 'OBSFREQ' 'XX' and
               'OBSFREQ' 'YY'


               TABCOL=3 1
               TABCOL=XBLPOS    COLSET=AURORA FREQ 2
               TABCOL=XBLPOS 1  COLSET=AURORA FREQ 2

               all return column 'xpos' in table 'XBLPOS' on subset
               level FREQ 2.

               Note that the keyword TABCOL= is only a generic keyword.




Keywords:

   INSET=      Give set (, subset(s)):
               1) Input is the name of a set (TOP= is asked (hidden)
                  also).
               2) Input is name of a set and a subset specification.
               For this INSET=, a 'table directory' will be generated.
               Maximum number of subsets is 4096.

** TOP=        Tables on top level only?             [Y]/N=All levels
               If INSET= has a set name only, it is possible to get
               tables on toplevel only (TOP=Y) or to get tables on
               all levels (TOP=N). This is the way to get display all
               the available columns.

   OPTION=     [1]=Exit,2=Set,3=Filter,4=Create,5=View,6=Plt,7=NEXT
               Select one of the options 1..14, enter 7 for next
               selection:
               8=Clc,9=Stat,10=Cor,11=Srt,12=Edt,13=Del,14=Cpy [back]
               Carriage return brings you back to the previous
               selection. The options are:

                1) Exit program
                2) Give new set or append one
                3) Set filter column
                4) Create (and fill) new column
                5) View, export (formatted) table data;
                6) Plot column(s)
                7) Next selection
                8) Fill column using expressions with columns as variables
                9) Elementary statistics on column data
               10) Correlation between two columns
               11) Sort a column
               12) Edit (and/or append to) a column
               13) Delete a column
               14) Copy contents of column to another column



               OPTION=1; Quit the option loop. quit Hermes:



               OPTION=2; Give a directory of tables and columns of
                         new (and old) set:


   SETAPPEND=  Append new tables?                                   [N]
               If you want the tables of a new set, use SETAPPEND=N,
               and if you want to append tables of a set in the table
               directory, use SETAPPEND=Y
               The number of subsets cannot exceed 4096 per set.

   INSET=      Give set (, subsets):



               OPTION=3; Setup a filter:


   FTABCOL=    Table, col. number for filter:              [Filter OFF]
               Carriage return results in inactivating the filter.
               A table (and column) number however will set up a
               filter array. This array need not to be of type LOG.
               Types REAL/DBLE and INT are also allowed. If in a
               REAL or DBLE type array a blank value is encountered,
               the corresponding filter value is set to FALSE.



               OPTION=4; Import data from ASCII file and put data
                         in Gipsy table.


   IMPORTFILE= Name of import ASCII file:                     [no file]

   OUTSET=     Give set (, subsets) to put column in:
               The number of subsets can be greater than 1.

   TABNAME=    Give name of table:
               Max. length is 8 characters.

   COLNAME=    Give name of column:
               Max. length is 8 characters.

   TYPE=       Type: INT REAL DBLE LOG CHARn:         [program default]
               Default depends on existence of destination.
               You must specify at least one of the characters
               I, R, D, L or C.

   NCHAR=      Give number of characters per string:               [10]
               Only asked if TYPE=4 (character type).

   COLSIZE=    Give number of entries in column:       [return to menu]
               Only asked if no import file was given, i.e.
               IMPORTFILE=<carriage return>

   UNITS=      Give units of column data:                        [none]
               Max. length is 8 characters.

   COMMENT=    Give comment:                                     [none]
               Max. length is 80 characters.

   APPCOL=     Append to existing column:                         [Y]/N
               If a selected column already exists, data can be
               appended at the end of the column with APPCOL=Y



               OPTION=5; Write contents of column(s) to screen,
                         ASCII file on disk or printer


   TABCOL*=    Table, column:                            [start output]
               The keyword is asked in a loop
               (TABCOL1=, TABCOL2= etc.) After one or more
               specifications, you can display the column(s) by
               pressing carriage return.
               First number is a table number, the second number is a
               column. If the column number is not specified, the
               first column of the table is used.

   SPACES=     Give number of spaces between columns:               [1]
               If more than one column is given, you can give the
               number of spaces between the columns.

   FORMAT*=    Give format X type:              [default for this type]
               For every column a format image is asked (FORMAT1=,
               FORMAT2= etc. With this image you specify field width
               and precision (see description). X is one of INT/REAL/
               DBLE/LOG/CHARn,  n is an integer.
               For the types REAL/DBLE there is an HMSn or DMSn
               format (See description).

   DESTINATION=[S]creen,  (F)ile,  (P)rinter,  (Q)uit          [Screen]
               Display your columns on screen or send it to a
               file (EXPORTFILE=) or send it to a printer (PRINTER=).
               The DESTINATION= keyword is asked in a loop which
               you can abort with DESTINATION=Q. It is also possible
               to give more than one destination f.i. DESTINATION=SFPQ
               will send output to screen, file and printer and quits
               the loop.

   EXPORTFILE= Name of export ASCII file:           [No output to file]
               If a name is specified, an ASCII file is created to
               store data. If you press carriage return, there will
               be no output to an ASCII file. If a given name already
               exists, APPEND= must be specified.

   APPEND=     File exists, ok to append?                         [Y]/N
               The file specified in EXPORTFILE= already exists.
               You can append to this file with APPEND=Y. If APPEND=N
               you will be prompted for another filename.

   PRINTER=    Give number of printer:
               First a list with available printers is generated



               OPTION=6; Create plot with contents of column as Y
                         (and X) values.


   GRDEVICE=   Plot device:                           [List of devices]
               Destination of plot, Screen or Hardcopy.

** PGMOSAIC=   View surface sub divisions in x,y:                 [1,1]
               View surface can contain a number of plots in
               in X and Y direction (mosaic). Default is 1 plot in
               both X- and Y direction.

** PGPAPER=    Give width(cm), aspect ratio:               [calc, calc]
               Aspect ratio is height/width. Default is calculated
               by the program and will obtain the largest view
               surface available.

** PGBOX=      Corners of box Xl,Yl,Xh,Yh:     [default by application]
               It is possible to overrule the calculated
               PGPLOT box size with PGBOX=. The coordinates (x,y) of
               the lower point are given first.

** PGCOLOR=    Give color 1..15:                                    [1]
               See description for the available colors.

** PGWIDTH=    Give line width 1..21:                               [1]

** PGHEIGHT=   Give character height:                             [1.0]

** PGFONT=     Give font 1..4:                                      [2]

   YTABCOL=    Table, col. for Y values:               [return to menu]
               Column representing Y values. If only a table
               number is given, the column number is 1 by default.
               The plotting loop is aborted by pressing carriage
               return.

   XTABCOL=    Table, col. for X values:              [Create X column]
               Column representing X values. If only a table
               number is given, the column number is 1 by default.
               If carriage return is pressed, the X array is filled
               with values as specified in XVALUES=

   XVALUES=    message, "Give %d values for X axis:        [calculated]
               Create a column with values for the X axis. The
               default creates values 1 .. [size of the Y column]

   EXTABCOL=   Table, col. for error in X values:      [No X error bar]
               The values in this column represent half the length
               of a total error bar in X direction.

   EYTABCOL=   Table, col. for error in Y values:      [No Y error bar]

   HEADER=     Give text as header above plot:                   [None]

   COMMENT=    Give text as comment in plot:               [No comment]

   COMPOS=     Give X, Y in plot coordinates:       [upper left corner]
               Position of comment.

** ANGLE=      Angle in degrees:                                    [0]
               Angle of comment, counted anti-clockwise.

** JUST=       Horz. justification 0.0 .. 1.0:                    [0.0]
               Horizontal justification is a number between 0 and 1.

** NEWFRAME=   Advance to new (plot) page?                        [Y]/N
               If NEWFRAME=N, next output will be in the same frame.

   IDPOS=      Enter position of id. in mm:                       [3 3]
               An identification string can be plotted at a 
               position x,y in mm.
   
   IDANGLE=    Enter angle of id. string (deg):                     [0]
               The identification string can be plotted at any 
               angle. The angle is entered in degrees.
   
   IDTXT=      Enter text for id.:                          [user,date]
               The identification string is either supplied by 
               the user or created by the program. The default string
               is created by the program and contains username and 
               date, e.g.
               GIPSY: k.u. keleku, Wed Jan 3 12:52:37 1996
               
  

               OPTION=7; Display the operations menu and ask for
                         a choice.


   OPTION=     8=Clc,9=Stat,10=Cor,11=Srt,12=Edt,13=Del,14=Cpy   [back]
               Another set of options from the main menu. Pressing
               carriage return displays the previous options.



               OPTION=8; Create a new column as a result of an
                         mathematical expression with variables
                         representing existing columns.


   EXPRESSION= F($1, $2,...$n) =                       [return to menu]
               Give an expression with $1..$n as variables
               representing columns. If the expression is approved by
               the program, the table, column specification is done
               with keyword TABCOLn= where n is a number corresponding
               with a variable.

               Keywords TABCOL*=, OUTSET=, TABNAME=, COLNAME=, UNITS=,
               COMMENT= and APPCOL= are described above.


               OPTION=9;  Elementary statistics on column data.


   STABCOL=    Table, col. for statistics:             [return to menu]
               Calculate statistics for specified column, i.e.
               -Number of entries in column
               -Minimum, and maximum of column data
               -Mean and rms of column data
               -Number of blanks in column.



               OPTION=10; Correlation between two columns.


   XTABCOL=    Table, col. for X values:               [return to menu]

   YTABCOL=    Table, col. for Y values:              [Create Y column]

   XVALUES=    Give n values for X axis:                   [calculated]
               Create a column with n values for the X axis. The
               default creates values: 1 ... [n=size of the Y column]

   HEADER=     Give text as header above plot:                   [None]

   PG ... =    If a plot device is opened, all PGplot keywords
               (PGMOSAIC=, PGBOX=, PGCOLOR= etc)
               can be specified as described above.

   ID ... =    Identification string keywords. See description at 
               OPTION=6
               
              

               OPTION=11; Sort a column.
               Not yet implemented.



               OPTION=12; Edit a column.


   ETABCOL=    Table, col. to edit data:               [Return to menu]

   ROW=        Give row(s) (1..13) to edit:                      [stop]
               Max. 1024 entries can be edited in one run.

   NEWVAL=     New 'type' value(s) at entry n:         [old value in n]
               'type' is one of INT/REAL/DBLE/LOG/CHAR and
               n is the row number of the entry. For all types except
               character data, it is possible to enter more than one
               value.



               OPTION=13; Delete columns and tables.


   DTABCOL=    Delete tab (, col(s)):                  [Return to menu]
               If you give one number, all columns will be deleted
               in that table. If more numbers are entered (max. of
               1024) then the first number is the table, and the
               other numbers are columns. Deleted columns are marked
               DELETED in the table directory.


               OPTION=14; Copy contents of column to another column.


   CTABCOL=    Table, column to copy:

               Keywords OUTSET=, TABNAME=, COLNAME=, UNITS=,
               COMMENT= and APPCOL= are described above.


Description:   The GIPSY application TABLE is an interactive program
               working with GIPSY tables. A GIPSY table is an entry in
               the descriptor of a set and is connected to set- or
               subset level. A table is identified by the set name,
               a subset level and a character string. Its contents is
               a list of column names, so a column can be specified by
               a set name (INSET=/OUTSET=), a subset level, a table name
               (TABNAME=), and a column name (COLNAME=). At the INSET=
               prompt you specify the name of the set and the subset
               level. If the subset level is omitted you can get
               tables from top level only OR get tables from all levels,
               i.e. all tables in the set. Top level is selected with
               the keyword TOP=Y (default), and all tables can be found
               with TOP=N.

               The column contains data with type INT, REAL, DBLE, LOG,
               or CHAR. Type, units, number of entries and a comment are
               also stored in the descriptor. The character type must
               include a length, so it will have a number as postfix
               (CHARn and n <= 132).

               The table directory:

               The first column is a table number that can be used to
               specify tables (instead of giving names). The second
               holds column numbers for each table. The corresponding
               names, type and units of column data and the size of a
               column are listed. 'Level' indicates the subset level
               where the column is found. If the set was a RA-DEC-FREQ
               set, then (* * 2) means the RA-DEC subset at frequency 2!
               The table directory is sorted in order of set name, table
               name, subset level and column name.

               With OPTION=2 you can extend this list with tables from
               other sets. This option is needed if one wants for
               instance to calculate a new column as function of two
               columns belonging to different sets.



               OPTION=3:

               The filter filters data in statistics, correlation,
               calculations (with OPTION=8), viewing and  plotting.
               A filter is set with OPTION=3 and FTABCOL=. The
               column can be of any type except character data.
               If the column has type INT, REAL or DBLE then the
               contents is converted to LOGical data in a way that
               all data not equal to zero is set to TRUE and all data
               equal to zero or blank data is set to FALSE. Note that
               the data in the filter array is not changed in any way.
               If a filter is active but the filter column is smaller
               than the column to be filtered, then the filter column
               is virtually extended with FALSE values. If a filter
               is active and a new column is created, then only the
               data that passed the filter is stored.



               OPTION=4:

               Importing data is possible with OPTION=4. First asked is
               the filename containing the ASCII data (IMPORTFILE=).
               If you press carriage return, the length of the new
               column is asked with COLSIZE= and the new column is
               filled with zeroes (use the edit option to fill ).

               If you have an import file, then this file must be an
               ASCII file which contains only one column. Character
               data consists of one or more text strings and a carriage
               return to limit the string.
               The LOG(ical) type is one of YES, JA, TRUE, NO, NEE,
               FALSE or abbreviations of these.

               The destination is needed (OUTSET=) which is the name of
               an existing set and the specification of subset(s). The
               table name (TABNAME=) and column name (COLNAME=) must be
               a unique combination. The type of the data (TYPE=) and
               the units (UNITS=) are character strings, not longer than
               8 characters. Some comment can be given (less than 80
               characters). The comment is displayed in the table
               directory and is listed right to the level information.
               If the column you wanted to create, already exists, it
               is possible to append data to this column with keyword
               APPCOL=Y


IMPORTANT:     Only the first column of an ASCII file is converted (except
               for character data) to data values.



               OPTION=5:

               In order to get the contents of a certain column, use
               OPTION=5. You specify a table and column number from the
               directory list and the program asks you if you want to
               direct the contents list to screen, to an ASCII file
               (EXPORTFILE=) or to a printer. If you give a file name and
               that file already exists, the program will prompt you with
               APPEND= The default is APPEND=Y, this appends column data
               to the existing file. You can create your own layout for
               one or more columns with the keywords FORMAT1=, FORMAT2= etc.
               Each keyword corresponds with a given column (TABCOL*=).

               The specification in FORMAT*= is called a 'format image'.
               A 'format image' is used to print numbers in a user given
               format consisting of characters representing the
               wanted output format. The syntax is:

 flag(s)       Zero or more flags, in any order, which modify the
               meaning of the conversion specification.  The flag
               characters and their meanings are:

      -        The result of the conversion is left-
               justified within the field.

      +        The result of a signed conversion always
               begins with a sign, "+" or "-".

 string        Characters, some with special meaning.
               If the string (f.i. FFFFF.FF or gggg.gg or wwwww)
               contains no dot, the number of characters specify
               a minimum field width.  For an output field, if the
               converted value has fewer characters than the field
               width, it is padded on the left (or right, if the
               left-adjustment flag, - has been given) to the field
               width.
               If the string contains a dot, the total number of
               characters including the dot is the minimum field width
               and the number of characters after the dot is the
               precision.

               The characters are used to determine the conversion
               type. If the string contains an:

               'e' or 'E'
                      The floating-point-number argument is
                      printed in the style [-]drddde+dd,
                      where there is one digit before the
                      radix character, and the number of
                      digits after it is equal to the
                      precision. The E conversion character
                      produces a number with E introducing
                      the exponent instead of e. The
                      exponent always contains at least two
                      digits.  However, if the value to be
                      printed requires an exponent greater
                      than two digits, additional exponent
                      digits are printed as necessary.

               'g' or 'G'

                      The floating-point-number argument is
                      printed in style f or e (or int style E
                      n the case of a G conversion
                      character), with the precision
                      specifying the number of significant
                      digits.  The style used depends on the
                      value converted; style e is used only
                      if the exponent resulting from the
                      conversion is less than -4 or greater
                      than or equal to the precision.

               others
                      Strings without 'e', 'E', 'g' and 'G'
                      indicate a floating point conversion.
                      The floating point number argument is
                      printed in decimal notation in the
                      style [-]dddrddd, where the number of
                      digits after the radix character, r, is
                      equal to the precision specification.

               If the result of a conversion is longer than the
               field width, an asterisk is returned. If the
               input number is a blank, a 'b' is returned.



Examples:      Format string: +eeeeee.eeee
                      Number: 43345.5436
                      Result:  +4.3346e+04
                      Remark: exponential format
                              signed conversion
                              field width: 12
                              precision:    4

               Format string: gggg.ggggg
                      Number: 34.43
                      Result:     34.430
                      Remark: Field width is 10
                              Number of significant digits is 5

               Format string: +ffff.ff
                      Number: 23.456
                      Result:   +23.46
                      Remark: signed conversion

               Format string: -ffff
                      Number: 345.87
                      Result: 346
                      Remark: left justified

               Format string: -+ffff.fff
                              Number: 34.43
                              Result: +34.430
                              Remark: left justified
                                      signed conversion

               Format string: eee.eeee
                      Number: 234.43
                      Result:        *
                      Remark: Field width too small
                              for conversion

               Format string: ffff.ff
                      Number: blank
                      Result:       b
                      Remark: input was a blank


               There is also a possibility to print REAL an DBLE numbers
               in HMS or DMS format. The length of the format is fixed.
               The precision of the seconds is given in HMSn or DMSn
               where n is an integer.


               OPTION=6;

               A simple plot routine allows you to plot the data in a
               column (YTABCOL=) as function of column entry or as
               function of another column (XTABCOL=). Pressing carriage
               return at the YTABCOL= prompt will abort the plot loop.
               Pressing carriage at the XTABCOL= prompt will give the
               keyword XVALUES= to create the X column yourself. The
               default for this keyword will result in an X axis with
               lowest value 1 and highest value equal to the length of
               the Y column. An expresssion can be given also. Error
               bars can be included with EXTABCOL= and EYTABCOL=

               If the size of YTABCOL= and XTABCOL= is
               different, the smallest size will be used as the number
               of column entries to plot. If the program can find the
               units of the data, it will plot these units along the
               axes. The keywords beginning with 'PG' all apply to
               plotting.

               There is some control over the plotting with the
               keywords PGMOSAIC= to get more plots on one page,
               PGPAPER= to set width of output and aspect ratio
               (i.e. height/width), PGBOX= to set your own plot box
               limits, PGCOLOR= to set the colour of frame and
               labels, PGFONT= to set the font style of all text in the
               plot, PGHEIGHT= to set the height of the characters. The
               default for PGHEIGHT=1.0. To get a height that is n times
               the current height, use PGHEIGHT=n.
               PGWIDTH= can be used to set the width of the lines.
               For overlays use NEWFRAME=N, so that new data will be
               plotted in the current plot. The data is scaled as
               previous data.


               Color indices for PGCOLOR=

               0      Background
               1      Default (Black if background is white)
               2      Red
               3      Green
               4      Blue
               5      Cyan
               6      Magenta
               7      Yellow
               8      Orange
               7      Yellow
               8      Orange
               9      Green + Yellow
               10     Green + Cyan
               11     Blue + Cyan
               12     Blue + Magenta
               13     Red + Magenta
               14     Dark Gray
               15     Light Gray
               16-255 Undefined

               Available fonts for PGFONT=

               1  single stroke "normal" font
               2  roman font
               3  italic font
               4  script font


               Text can be added to the plot with the keywords:
               COMMENT= which accepts strings, COMPOS= which needs
               two numbers to position the text in COMMENT=
               (the default is the upper left corner).
               The position is in world coordinates. The string is
               drawn with the baseline of all the characters passing
               through COMPOS=
               The positioning of the string along this line is
               controlled by JUST=. If JUST=0.0, the string will be
               left-justified at the point COMPOS=); if JUST=0.5, it
               will be centered, and if JUST=1.0, it will be right
               justified. [Other values of JUST= give other justifi-
               cations.]. The text can be
               written at an arbitrary angle with ANGLE=. The angle
               (in degrees) increases counter-clockwise where
               ANGLE=0.0 is horizontal.



               OPTION=8;


               OPTION=8 deals with expressions which create a new
               column as a function of existing columns.
               The existing columns corresponding with the variables
               $1 .. $n, can be of type INT, REAL or DOUBLE. The result
               however is always of type REAL. The length of the
               result column is equal to the size of the smallest column.
               The destination of the result column must be specified
               with keywords OUTSET=, TABNAME= etc.


               Description of the syntax in the expressions:

             1) functions; syntax ff(..); where ff is one of the
                following available functions:
                 abs(x)         absolute value of x
                 sqrt(x)        square root of x
                 sin(x)         sine of x
                 asin(x)        inverse sine of x
                 cos(x)         cosine of x
                 acos(x)        inverse cosine of x
                 tan(x)         tangent of x
                 atan(x)        inverse tan of x
                 atan2(x,y)     inverse tan (mod 2pi) x = sin, y = cos
                 exp(x)         exponential of x
                 ln(x)          natural log of x
                 log(x)         log (base 10) of x
                 sinh(x)        hyperbolic sine of x
                 cosh(x)        hyperbolic cosine of x
                 tanh(x)        hyperbolic tangent of x
                 rad(x)         convert x to radians
                 deg(x)         convert x to degrees
                 erf(x)         error function of x
                 erfc(x)        1-error function
                 max(x,y)       maximum of x and y
                 min(x,y)       minimum of x and y
                 sinc(x)        sin(x)/x (sinc function)
                 sign(x)        sign of x (-1,0,1)
                 mod(x,y)       gives remainder of x/y
                 int(x)         truncates to integer
                 nint(x)        nearest integer
                 ranu(x,y)      generates uniform noise between x and y
                 rang(x,y)      generates gaussian noise with mean x
                                and dispersion y
                 ranp(x)        generates poisson noise with mean x
                 ifeq(x,y,a,b)  returns a if x == y, else b
                 ifne(x,y,a,b)  returns a if x != y, else b
                 ifgt(x,y,a,b)  returns a if x > y, else b
                 ifge(x,y,a,b)  returns a if x >= y, else b
                 iflt(x,y,a,b)  returns a if x < y, else b
                 ifle(x,y,a,b)  returns a if x <= y, else b
                 ifblank(x,a,b) returns a if x == BLANK, else b
              2) constants; syntax cc; where cc is one of the following
                 available constants:
                 PI             3.14159....
                 C              speed of light (SI)
                 H              Planck (SI)
                 K              Boltzmann (SI)
                 G              gravitation (SI)
                 S              Stefan-Boltzman (SI)
                 M              mass of sun (SI)
                 P              parsec (SI)
                 BLANK          Universal undefined value
                 Note: the Hubble constant is not included.
              3) operators; syntax op; where op is one of the following
                 available operators:
                 +              addition
                 -              subtraction
                 *              multiplication
                 /              division
                 **             power
              4) parameters; syntax $n; where 1 <= n <= 32.

              A) The calculations are all done in double precision.

              B) BLANK values are recognized. Any operation on a
                 BLANK causes the result to be set to BLANK (except the
                 function IFBLANK). Also all illegal operations, like
                 dividing by zero or the logarithm of a negative value,
                 will cause the result to be set to BLANK.



               OPTION=9;

               Statistics.

               For the column given in TABCOL= the program calculates
               the minimum and maximum value, the mean and rms of the
               data. Also the number of blanks are printed.


               OPTION=10;

               Correlation and regression.

               The correlation option first plots a diagram with pixel
               values of the two given columns (same remarks as with
               the plot option wrt. creating your own temporary column
               for the X axis). Furthermore, it
               calculates the regression lines y=ax+b and x=cy+d, the
               deviations from these lines and the linear correlation
               coefficient r. If r = 1 or r = -1, there is perfect
               positive resp. negative correlation. If r = 0, there is
               total absence of correlation.
               If n is the number of valid data pairs in the regression,
               the significance of r is the probability Pc(r,n) that
               the data points could represent a sample from an
               uncorrelated parent population:
               Pc(r,n) < 5%   is significant.
               Pc(r,n) < 1%   is highly significant.
               Pc(r,n) < 0.1% is very highly significant.

               In most cases with large n, this probability is so
               small, that 0(%) is printed.
               The applied statistic t = r * sqrt( n-2 / 1-r**2 ) is
               distributed in the case of the null-hypothesis (of no
               correlation) like Student's t-distribution with
               v = n-2 degrees of freedom.
               The two sided significance level is given by Pc(r,n).

               Example:

               Statistics
               ==========
               Data points diagram  : n = 100
               Regr. Y on X    : Y = +0.9804 (+/-0.0357) X + -2.1644 (+/-0.3035)
               Regr. X on Y    : X = +0.9026 (+/-0.0328) Y + +1.9823 (+/-0.2978)
               Linear corr. coeff.  : r = 0.940727
               Significance of r    : Pc(r,n) = 0.0(%)

               Pc(r,n) < 5%   is significant.
               Pc(r,n) < 1%   is highly significant.
               Pc(r,n) < 0.1% is very highly significant.



               References:

               -Kreyszig, E., Introductory Mathematical Statistics
                Chapter 17, 18.
               -Press, H. et al, Numerical Recipes, Chapter 14.
               -Bevington, P., Data Reduction and Error Analysis
                for the Physical Sciences, Chapter 7.
               -Ractliffe, J., Elements of Mathematical Statistics
                Chapter 16.


               OPTION=11;

               Sorting a column. Not yet implemented


               OPTION=12;

               A column must be given with ETABCOL= The length of this
               column is the maximum number of entries you can edit.
               The keyword ROW= specifies the entries in this column
               that you want to edit.
               Editing is done in a loop with the keyword NEWVAL=
               as the new value. The default is the old value. For
               all types except character data, you can give more
               than one value at this prompt. If, for example, you
               have a column with at least 91 entries and want the
               entries to be filled with values SIN(0) to SIN(90)
               (angle in degrees), use the expression:

                         NEWVAL=SIN(RAD([0:90:1]))

               The repeat argument [0:90:1] generates the numbers
               0, 1, 2,... 90. These numbers are converted to radians
               and after calculating the sine, they are stored in the
               column.

               Editing can be done also by exporting the data to an
               ASCII file (OPTION=5). Edit the data with your editor
               and read the data back with OPTION=4.


               OPTION=13;

               Deleting can be of columns or tables. If DTABCOL=
               contains one name or number, it refers to a table.
               If it contains more than one, the first one specifies
               the table and the others the columns in the table.
               After deleting columns, the columns are marked
               DELETED in the table directory. They are deleted in
               the set descriptor, but remain in the table directory
               until a new set is used.


               OPTION=14;

               Copy data to another column. Only one column can be
               copied at a time. INT/REAL/DBLE/LOG data can be copied
               to INT/REAL/DBLE/LOG data. Character data can only be
               copied to character data, but the number of characters
               can decrease. For example if type was CHAR20 you can
               select TYPE=CHAR10




Notes:

Example:       Program FINDGAUSS created a number of columns in set
               AURORA. With the default file cols.def, the next table
               was printed:

        X centre           Y centre      Flux     Major     Minor        Pa
           (hms)              (dms)    Jy/pix         "         "    degree
===========================================================================
   3h 17m  8.43s      41d 18m  2.7s    10.687    18.799    12.800   178.689
   3h 16m 29.90s      41d 18m 40.7s     1.584    31.699    25.663   144.585
   ....               ....

               Contents cols.def:

               INSET=a 0
               OPTION=5
               SPACES=2
               TABCOL1=1 11
               FORMAT1=hms2
               COLNAME1=X centre
               COLUNITS1=(hms)
               TABCOL2=1 16
               FORMAT2=dms1
               COLNAME2=Y centre
               COLUNITS2=(dms)
               TABCOL3=1 1
               FORMAT3=
               COLNAME3=Flux
               COLUNITS3=Jy/pix
               TABCOL4=1 5
               FORMAT4=
               COLNAME4=Major
               COLUNITS4="
               TABCOL5=1 7
               FORMAT5=
               COLNAME5=Minor
               COLUNITS5="
               TABCOL6=1 3
               FORMAT6=
               COLNAME6=Pa
               COLUNITS6=degree







Updates:       Jun 11,  1992: VOG, Document created.
               Jul 10,  1992: KGB, increased length of comment, etc.
               Aug 24,  1992: VOG, Second release.
               Okt  5,  1992: VOG, Subset selection possible,
                                   HMS/DMS formats added
               Apr 12,  1995: VOG, Removed code around "PGLAB"
               Oct 20,  1995: VOG, Increased length of 'messbuf2'
                                   (TABLE crashed when displaying
                                   character data).
               Jan 03,  1996: VOG, Added identification string in plots.

#<
*/


/*  table.c: include files     */

#include    "stdio.h"        /* Defines ANSI C input and output utilities */
#include    "stdlib.h"       /* Defines the ANSI C functions for number */
                             /* conversion, storage allocation, and similar tasks.*/
#include    "string.h"       /* Declares the ANSI C string functions*/
                             /* like:strcpy, strcat etc.*/
#include    "math.h"         /* Declares the mathematical functions and macros.*/
#include    "cmain.h"        /* Defines the main body of a C program with */
                             /* MAIN_PROGRAM_ENTRY and IDENTIFICATION */
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for Fortran to C intface */
                             /* including def. of char2str,str2char,tofchar,zadd */
                             /* and macros tobool and toflog */
#include    "float.h"        /* Definition of FLT_MAX etc.*/
#include    "ctype.h"        /* Declares ANSI C functions for testing characters */
                             /* like: isalpha, isdigit etc. also tolower, toupper.*/

/* Miscellaneous */

#include    "init.h"         /* Declare task running to HERMES and initialize.*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans up the mess.*/
#include    "anyout.h"       /* General character output routine for GIPSY programs.*/
#include    "setfblank.h"    /* Subroutine to set a data value to the universal BLANK.*/
#include    "setdblank.h"    /* Subroutine to set a data value to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "getusernam.h"   /* Returns the user name of the current user.*/
#include    "getdate.h"      /* Returns the current time and date as a text string */
#include    "fieini.h"
#include    "fiedo.h"
#include    "printusing.h"
#include    "minmax1.h"
#include    "statr.h"
#include    "hms.h"
#include    "dms.h"
#include    "dcdint.h"


/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"
#include    "userchar.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/

/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gds_exist.h"    /* Test whether set exists. */
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/

/* PGPLOT includes */

#include    "pgplot.h"

/* Table includes */

#include "gdsa_crecol.h"
#include "gdsa_tablis.h"
#include "gdsa_tabinq.h"
#include "gdsa_colinq.h"
#include "gdsa_deltab.h"
#include "gdsa_delcol.h"
#include "gdsa_rcint.h"
#include "gdsa_rcreal.h"
#include "gdsa_rcdble.h"
#include "gdsa_rcchar.h"
#include "gdsa_rclog.h"
#include "gdsa_wcint.h"
#include "gdsa_wcreal.h"
#include "gdsa_wcdble.h"
#include "gdsa_wcchar.h"
#include "gdsa_wclog.h"

/* Printer related */

#include "prntrnum.h"          /* Total number of printers */
#include "prntrcom.h"          /* Find comment for this printer */
#include "prntrdim.h"          /* Find number of columns and rows for printer */
#include "prntrnam.h"          /* Find name of printers */
#include "prntract.h"          /* Send file to printer */



/* DEFINITIONS: */

/* Initialize Fortran compatible string with macro 'fmake' */

#define fmake(fchr,size) { \
                           static char buff[size+1]; \
                           int i; \
                           for (i = 0; i < size; buff[i++] = ' '); \
                           buff[i] = 0; \
                           fchr.a = buff; \
                           fchr.l = size; \
                         }

/* Malloc version of 'fmake'  */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( a * 0.017453292519943295769237 )
#define DEG(a)         ( a * 57.295779513082320876798155 )

#define RELEASE        "2.0"           /* Version number */
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS     4096            /* Max. allowed subsets */
#define MAXBUF         4096            /* Buffer size for I/O */
#define MAXPARS        32              /* Max number of parameters in expression */
#define KEYLEN         20              /* Max length of keywords */
#define NCHAR          132             /* Max width of character column */
#define NONE           0               /* Default level in userxxx routines */
#define REQUEST        1               /* " */
#define HIDDEN         2               /* " */
#define EXACT          4               /* " */
#define YES            1               /* C versions of .TRUE. */
#define NO             0               /* and .FALSE. */
#define VARLEN         132             /* Length of variable GDS records */
#define LONGLEN        120             /* Buffer size for text buffers */
#define NORMLEN        80
#define SHORTLEN       8
#define MAXDELETE      1024            /* Max. number of tab/col combinations to delete */
#define MAXDISPLAY     256             /* Max. number of columns that can be printed */
#define MAXTABBUF      256             /* Buffer size to hold string for display */
#define MAXEDIT        1024            /* Max. number of entries in column to edit in one run */
#define HMSLEN         14              /* Fixed minimum space for hms format */
#define DMSLEN         16              /* Fixed minimum space for dms format */
#define INITCOLNUM     128             /* First time allocation of columns */


/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     nsubs;              /* Number of input subsets */
static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered ccording to the */
                                    /* specification by the user. */
static fint     showdev;            /* Device number (as in ANYOUT) for info */
static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
                                    /* number of grids along an axes as */
                                    /* specified by the user. The first elements */
                                    /* (upto the dimension of the subset) contain */
                                    /* the length of the subset axes, the other */
                                    /* ones contain the the number of grids along */
                                    /* an axes outside the subset. */
static fint     maxsubs = MAXSUBSETS;
static fint     maxaxes = MAXAXES;  /* Max num. of axes the program can deal with.*/
static fint     class = 1;          /* Class 1 is for applications which repeat */
                                    /* the operation for each subset, Class 2 */
                                    /* is for applications for which the operation */
                                    /* requires an interaction between the different */
                                    /* subsets. */
static fint     subdim;             /* Dimensionality of the subsets for class 1 applications */


/* PGPLOT variables */

const  fint  background  =  0;      /* Color definitions for PGPLOT. */
       fint  foreground  =  1;      /* Black if background is white. */
       fint  red         =  2;
       fint  green       =  3;
       fint  blue        =  4;
const  fint  cyan        =  5;
const  fint  magenta     =  6;
       fint  yellow      =  7;
const  fint  orange      =  8;
const  fint  greenyellow =  9;
const  fint  greencyan   = 10;
const  fint  bluecyan    = 11;
const  fint  bluemagenta = 12;
const  fint  redmagenta  = 13;
const  fint  darkgray    = 14;
const  fint  lightgray   = 15;

static fint  symbol      =  2;      /* Take a plus as plot symbol, see PGPLOT MANUAL */



typedef struct {
   char  setname[LONGLEN+1];          /* Table belongs to this set, DO NOT CHANGE! */
   char  tablename[SHORTLEN+1];       /* Name of table, DO NOT CHANGE! */
   char  columnname[NORMLEN+1];       /* Name of column, DO NOT CHANGE! */
   char  comment[VARLEN+1];           /* Comment for this column */
   char  type[VARLEN+1];              /* Type of data, DO NOT CHANGE! */
   char  units[VARLEN+1];             /* Units of data */
   char  level[NORMLEN+1];            /* Subset level (in text) of table, column */
   char  format[NORMLEN+1];           /* Format numbers with this format image */
   char  header[NORMLEN+1];           /* Text above column in output */
   int   subset;                      /* Subset level, DO NOT CHANGE! */
   int   size;                        /* Number of entries in column */
   int   tabnr;
   int   colnr;
} columnstruct;

columnstruct  *column = NULL;       /* Allocate dynamically room for structures */


/* Miscellaneous */

static fchar    Key, Mes;
static float    Fblank;             /* Global float value for BLANK. */
static double   Dblank;             /* Global double value for BLANK. */
static char     message[LONGLEN];   /* All purpose character buffer. */
static int      i;                  /* Various counters. */
static bool     agreed;             /* Loop guard. */
FILE            *exportfile;        /* File pointer to ASCII file */
FILE            *importfile;
static float    *floatarray  = NULL;
static fint     *fintarray   = NULL;
static double   *doublearray = NULL;
static fchar    fchararray;
static bool     *boolarray = NULL;
static float    *dataIN  = NULL;
static float    *dataOUT = NULL;
static bool     *filterarray = NULL;
static int      filtersize;
static int      filtercol;
static bool     filter = NO;
static bool     opendisplay = NO;
static fint     numsubs;
static fint     insubs[MAXSUBSETS];  /* Subset coordinate words */



typedef    float  *floatptr;


static void anyoutC( int dev, char *anyCstr )
/*------------------------------------------------------------------*/
/* The C version of 'anyout_c' needs two parameters:                */
/* an integer and a C-type string. The integer determines           */
/* the destination of the output which is:                          */
/*    0  use default [set by HERMES to 3 but can be changed by user]*/
/*    1  terminal                                                   */
/*    2  LOG file                                                   */
/*    8  terminal, suppressed in "experienced mode"                 */
/*   16  terminal, only when in "test mode"                         */
/*------------------------------------------------------------------*/
{
   fint ldev = (fint) dev;
   anyout_c( &ldev, tofchar( anyCstr ) );
}



static int prnmenu( fchar keyword, fint *scrnum, fint *printer,
                    fint *cols, fint *rows )
/*----------------------------------------------------------------------
 * Use:       i = prnmenu(  keyword, Input   fchar
 *                          scrnum,  Input   * fint
 *                          printer, Input   * fint
 *                          cols,    Output  * fint
 *                          rows     Output  * fint
 *                       )
 *
 *                prnmenu   Number of chosen printer.
 *                scrnum    Destination of menu info. Same numbers as
 *                          used in ANYOUT routine.
 *                printer   Suggested number of a printer
 *                cols      Number of columns of selected printer.
 *                rows      Number of rows of selected printer.
 *
 * Description:   If user given printer number does not correspond to
 *                an available printer (available under current
 *                system), present menu and ask user to select a printer.
 *                Return the number of this printer and its number of
 *                rows and columns.
 *                Use the function 'prntract' in the calling environment
 *                to send a file to the selected printer.
 *----------------------------------------------------------------------
 */
{

#define MAXPRNS 20


   /*  Miscellaneous: */

   fint            Ires1, Ires2;
   fint            prnnum;             /* Number of selected printer */
   fchar           prnname;            /* Name of selected printer */
   fchar           prncom;             /* Comments for this printer */
   fint            prncol[MAXPRNS];    /* Number of columns and rows for ... */
   fint            prnrow[MAXPRNS];    /* ... all available printers */
   char            txt[120];           /* Buffer for string manipulations */
   fchar           message;
   fint            dfault;
   fint            valid;              /* Is printer accepted ? */
   fint            i, j;                  /* Counter */
   fint            numitems;
   int             printerindex;


   fmake( message, 80 );
   fmake( prnname, 40 );
   fmake( prncom,  60 );
   prnnum = prntrnum_c();              /* Get number of available printers... */
                                       /* in current system */
   if (prnnum < 0) {                   /* Something went wrong */
      switch((int)prnnum) {
         case -1:
            (void) strcpy( txt, " Cannot obtain hostname" );
            break;
         case -2:
            (void) strcpy( txt,
            " Cannot obtain translation of printer description file!" );
            break;
         case -3:
            (void) strcpy( txt, " Cannot open printer description file!" );
            break;
         case -4:
            (void) strcpy( txt, " Cannot allocate enough space!" );
            break;
         case -5:
            (void) strcpy( txt, " Printer comment exceeds buffer length!" );
            break;
         case -6:
            (void) strcpy( txt, " Printer name exceeds buffer length!" );
            break;
         case -7:
            (void) strcpy( txt, " Printer comment exceeds buffer length!" );
            break;
         case -8:
            (void) strcpy( txt, " Cannot obtain number of columns!" );
            break;
         case -9:
            (void) strcpy( txt, " Cannot obtain number of rows!" );
      }
      anyout_c( scrnum, tofchar(txt) );
      return( 0 );                     /* Abort action, return to ... */
                                       /* ... calling environment */
   }
   for (i = 0, j = 1;  i < prnnum; i++, j++) {     /* Get rows and columns */
     Ires1 = prntrdim_c( &j,
                         &prncol[i],
                         &prnrow[i] );
     if (Ires1 != 0) {                 /* Fill with dummy if no columns ... */
        prncol[i] = -1;                /* ... and rows could be found */
        prnrow[i] = -1;
     }
   }
   if ((*printer <= 0) || (*printer > prnnum)) {
      (void) strcpy( txt,
      " ==============================PRINTERS=============================" );
                                       /* VAX C does not concatenate broken...*/
                                       /* strings, so keep it on one line */
      anyout_c( scrnum, tofchar(txt) );
      (void) sprintf( txt, " %3.3s   %-20.20s  %4.4s  %4.4s  %-40.40s" , "nr",
              "    name", "cols", "rows", "   comment" );
      anyout_c( scrnum, tofchar(txt) );
      (void) strcpy(txt,
      " ===================================================================" );
      anyout_c( scrnum, tofchar(txt) );
      for (i = 0, j = 1; i < prnnum; i++, j++) {
          Ires1 = prntrnam_c( &j, prnname );
          Ires2 = prntrcom_c( &j, prncom );
          if (Ires1 != 0) (void) strcpy(prnname.a, "****");  /* No name available */
          if (Ires2 != 0) (void) strcpy(prncom.a,  "****");  /* No comment available */
          if (prncol[i] < 0) {
             (void) strcpy( txt, "No information about this printer" );
          }
          else {
             (void) sprintf( txt, " %3d   %-20.20s  %4d  %4d  %-40.*s", j, prnname.a,
                             prncol[i], prnrow[i], nelc_c(prncom), prncom.a );
          }
          anyout_c( scrnum, tofchar(txt) );
      }
      (void) strcpy( txt,
      " ===================================================================" );
      anyout_c( scrnum, tofchar(txt) );
      anyout_c( scrnum, tofchar("  ") );
      cancel_c( keyword );
      dfault = NONE;                   /* Change the default */
      message = tofchar("Give number of printer: ");
      do {
         numitems = 1;
         Ires1  = userint_c( printer, &numitems, &dfault, keyword, message );
         printerindex = *printer - 1;
         valid = ( (*printer > 0) && (*printer <= prnnum) &&
                   (prncol[printerindex] > 0) );
         if (!valid) {
            cancel_c( keyword );
            anyout_c( scrnum, tofchar("Selection not possible!") );
            message = tofchar("Try again: Select a printer:");
         }
      } while (!valid);
   } else {
      printerindex = *printer - 1;
   }
   *cols = prncol[printerindex];
   *rows = prnrow[printerindex];
   return ( *printer );                /* Return the number of the printer */
}




static void FtoCstr( fchar fstr, char *cstr, int maxlen )
/*---------------------------------------------*/
/* Copy at most 'maxlen' characters from       */
/* the Fortran type string to the C string.    */
/* The string is created with 'fmake' (which   */
/* reserves one extra space for the null char. */
/*---------------------------------------------*/
{
   int  len;
   int  j;

   len = nelc_c( fstr );
   len = MYMIN(len, maxlen );
   for (j = 0; j < len; j++) cstr[j] = fstr.a[j];
   cstr[j] = '\0';
}



static void clearstr( fchar Fstr )
/*---------------------------------------------*/
/* Blank a Fortran string up to Len characters */
/*---------------------------------------------*/
{
   int    i;
   fint   len = Fstr.l;


   for (i = 0; i < (int) len; i++) {
      Fstr.a[i] = ' ';
   }
}


static void displayfilter( int filtercol, int excluded )
/*------------------------------------------------------------*/
/*------------------------------------------------------------*/
{
   int     scr = 1;

   sprintf( message,
           "FILTER=[%.*s, %.*s] has LENGTH %d and TYPE %.*s and EXCLUDED %d entries",
            MYMIN(8, strlen(column[filtercol].tablename)),
            column[filtercol].tablename,
            MYMIN(8, strlen(column[filtercol].columnname)),
            column[filtercol].columnname,
            column[filtercol].size,
            MYMIN(8, strlen(column[filtercol].type)),
            column[filtercol].type,
            excluded );
   anyoutC( scr, message );
}


static int filterreal( float *realdata, int ndat )
/*-----------------------------------------------------------*/
/* Filter array according to logicals in array 'filterarray' */
/*-----------------------------------------------------------*/
{
   int      v, w;

   for(w = 0, v = 0; w < MYMIN(filtersize, ndat); w++) {
      if (filterarray[w]) realdata[v++] = realdata[w];
   }
   return(v);
}


static void displaytableinfo( int k )
/*--------------------------------------------------*/
/* Give short message about column k                */
/*--------------------------------------------------*/
{
   char     display[132];

   (void) sprintf(   display,
   "COLUMN: [%.*s, %.*s], TYPE: [%s], SIZE: [%d], UNITS: [%s]",
                     MYMIN(8, strlen(column[k].tablename)),
                     column[k].tablename,
                     MYMIN(8, strlen(column[k].columnname)),
                     column[k].columnname,
                     column[k].type,
                     column[k].size,
                     column[k].units );
   anyoutC( 1, display );
}



FILE *openfile( char *filename )
/*-----------------------------------------------------*/
/* Open file for writing. Ask filename in GIPSY way    */
/* Check file for existence. Return file pointer       */
/* and the name of the given file.                     */
/* The function introduces the keywords EXPORTFILE= and  */
/* APPEND=. The macro 'fmake' and the definitions for  */
/* YES and NO must be available.                       */
/*-----------------------------------------------------*/
{
#include    "stdio.h"
#include    "usertext.h"
#include    "userlog.h"
#include    "cancel.h"
#include    "reject.h"

#define    NAMELEN    80
#define    KEYLEN     20

   fchar     Filename;
   bool      append;
   fint      request = 1;
   fint      dfault;
   fint      nitems;
   fint      agreed;
   fint      n;
   fchar     Key, Mes;
   FILE     *fp;

   dfault = request;
   fmake( Filename, NAMELEN );
   fmake( Key, KEYLEN );
   fmake( Mes, NAMELEN );
   do {
      append = toflog(YES);                  /* Default APPEND=Y */
      Key    = tofchar("EXPORTFILE=");
      Mes    = tofchar("Name of export ASCII file:     [No output to file]");
      n      = usertext_c( Filename,
                           &dfault,
                           Key,
                           Mes );
      if (n == 0) return NULL;
      (void) strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */
      fp = fopen(filename, "r");
      if (fp != NULL) {
         nitems = 1;
         Key = tofchar("APPEND=");
         Mes = tofchar("File exists, ok to append?    [Y]/N");
         n   = userlog_c( &append,
                        &nitems,
                        &dfault,
                        Key,
                        Mes );
         append = tobool( append );
         fclose( fp );
         cancel_c( Key );
      }
      Key = tofchar("EXPORTFILE=");
      if (!append) {
         cancel_c( Key );
         agreed = NO;
      } else {
         fp = fopen(filename, "a");
         agreed = (fp != NULL);
         if (!agreed) {
            reject_c( Key, tofchar("Cannot open, try another!") );
         }
      }
   } while (!agreed);
   cancel_c( Key );
   return( fp );                /* Return the file pointer */
}



static int gettabcols( fchar Key, fchar Mes, fint dfault1, int maxcols,
                       int currentcols,
                       int  *columnarray, char *tablename, bool *tabonly )
/*----------------------------------------------------------------------*/
/* Table and column can be identified by the table- and column numbers  */
/* as given in the so called 'table directory'. This is the list with   */
/* all the names and levels etc. This routine identifies columns. It    */
/* returns the number of columns it could find for a certain input and  */
/* it returns for each column the corresponding index in the column-    */
/* structure. First item in an input is always a table, other items     */
/* specify up to 'maxcols' columns. If only a table is given, it        */
/* all columns that belong to this table, but it sets also a flag that  */
/* only a table was entered (*tabonly=YES). It is possible to enter     */
/* tables and columns by their names also. A name is prefixed by the    */
/* " character. Names and numbers can be mixed. However for names,      */
/* there can be columns with the same name belonging to different       */
/* sets or subset levels. The user is prompted then with a keyword to   */
/* specify set and subset. */
/*----------------------------------------------------------------------*/
{
#define MAXINPUTLEN      16
   fint     r1, r2, r3;
   fint     dfault;
   fchar    Tabcolnames;
   char     *charbuf = NULL;
   int      buflen;
   int      i, j, k;
   fint     nitems;
   fint     *numbers = NULL;
   fint     num;
   bool     agreed;
   char     tabname[MAXINPUTLEN+1];
   char     colname[MAXINPUTLEN+1];
   fint     tablenumber;
   char     ch;
   char     dcdstr[MAXINPUTLEN+1];
   int      columnsfound;
   bool     first;
   int      subset = 0;
   char     nameofset[LONGLEN+1];
   bool     unique;
   bool     tabnameexist;
   bool     tabnumexist;
   bool     setasked;
   fchar    Key2, Mes2;
   char     setname[LONGLEN+1];


   fmake( Key2, 20 );
   fmake( Mes2, 80 );
   /* Space for 1 table name and 'maxcols' columns and spaces */
   buflen  = (MAXINPUTLEN) * (maxcols+1) + maxcols;
   charbuf = (char *) calloc( buflen, sizeof(char) );
   if (charbuf == NULL) {
      reject_c( Key, tofchar("Cannot allocate memory!") );
      return( 0 );
   }
   nitems  = maxcols + 1;
   numbers = (fint *) calloc( maxcols + 1, sizeof(fint) );
   if (numbers == NULL) {
      reject_c( Key, tofchar("Cannot allocate memory!") );
      free( charbuf );
      return( 0 );
   }


   do {
      first         = YES;
      *tabonly      = NO;
      columnsfound  = 0;
      num = maxcols + 1;
      for (i = 0; i < buflen; i++) charbuf[i] = ' ';
      Tabcolnames.l = MAXINPUTLEN;
      Tabcolnames.a = charbuf;
      r1            = userchar_c( Tabcolnames, &nitems, &dfault1, Key, Mes );
      if (r1 == 0) {
         free( numbers );
         free( charbuf );
         return( 0 );
      }

      /* Copy first entry to table name */

      i = 0;                                                     /* Examine first entry */
      ch = Tabcolnames.a[i*MAXINPUTLEN];
      for (j = 0; ((j < MAXINPUTLEN) && (ch != ' ')); j++) {     /* copy name */
         tabname[j] = ch;
         ch = Tabcolnames.a[i*MAXINPUTLEN+j+1];
      }
      tabname[j] = '\0';


      /* Does this table exist (or is it a number */

      tabnameexist = NO;
      setasked     = NO;
      columnsfound = 0;
      first        = YES;
      for (k = 0, unique = YES; ((k < currentcols) && unique); k++) {
         if ( strcmp(column[k].tablename, tabname) == 0 ) {
            if (first) {
               strcpy( nameofset, column[k].setname );
               subset   = column[k].subset;
               first    = NO;
               tabnameexist = YES;
               columnsfound = 1;
            } else {
               unique = ( (strcmp(column[k].setname, nameofset) == 0) &&
                          (column[k].subset == subset) );
               if (unique) columnsfound++;
            }
         }
      }
      columnsfound = 0;
      tabnumexist = NO;
      if (!tabnameexist) {       /* It could be a table number! */
         num = 1;
         r2  = dcdint_c( tofchar(tabname), &tablenumber, &num, &r3 );
         if (r3 >= 0) {
            tabnumexist = YES;
         }
      }
      if (tabnumexist) {
         first = YES;
         for (k = 0; k < currentcols; k++) {
            if (column[k].tabnr == (int) tablenumber) {
               if (first) strcpy( tabname, column[k].tablename );
               /* Only store if entry fits in array */
               if (columnsfound < maxcols) columnarray[columnsfound] = k;
               columnsfound++;
            }
         }
         tabnumexist = (columnsfound > 0);
      }
      agreed = (tabnameexist || tabnumexist);
      if (!agreed) {
         reject_c( Key, tofchar("Cannot find table!") );
      } else {
         if (tabnameexist && !unique) {
            /* Ask for set, subset */
            dfault = NONE;
            subdim = 0;
            Key2   = tofchar("COLSET=");
            Mes2   = tofchar("Set (one subset) for table:");
            showdev= 3;
            nsubs  = gdsinp_c( Setin, subin, &maxsubs, &dfault, Key2, Mes2,
                               &showdev, axnum, axcount, &maxaxes, &class, &subdim );
            cancel_c( Key2 );
            FtoCstr( Setin, setname, LONGLEN );
            subset   = subin[0];
            setasked = YES;
         }
         if (tabnameexist) {
            if (r1 == 1) {
               for (k = 0; k < currentcols; k++) {
                  if ( strcmp(column[k].tablename, tabname) == 0 ) {
                     if (setasked) {
                        if ( (strcmp(column[k].setname,   setname) == 0) &&
                             (column[k].subset == subset) ) {
                           if (columnsfound < maxcols) columnarray[columnsfound] = k;
                           columnsfound++;
                        }
                     } else {
                        if (columnsfound < maxcols) columnarray[columnsfound] = k;
                        columnsfound++;
                     }
                  }
               }
            }
         }
         strcpy( tablename, tabname );
         if (r1 == 1) {
            /* Only the table was given */
            *tabonly = YES;
            break;                            /* leave while loop and function */
         }

         /* Also columns were specified */

         columnsfound = 0;
         num = (fint) maxcols;
         for (i = 1; i < r1; i++) {
            ch = Tabcolnames.a[i*MAXINPUTLEN];
            for (j = 0; ((j < MAXINPUTLEN) && (ch != ' ')); j++) {     /* copy name */
               colname[j] = ch;
               ch = Tabcolnames.a[i*MAXINPUTLEN+j+1];
            }
            colname[j] = '\0';
            /* These can be columns only */
            for (k = 0; k < currentcols; k++) {
               if ( (strcmp(column[k].tablename,  tabname) == 0) &&
                    (strcmp(column[k].columnname, colname) == 0) ) {
                  if (setasked) {
                     if ( (strcmp(column[k].setname, setname) == 0) &&
                          (column[k].subset == subset) ) {
                        if (columnsfound < maxcols) {
                           columnarray[columnsfound] = k;
                           num--;
                        }
                        columnsfound++;
                     }
                  } else {
                     if (columnsfound < maxcols) columnarray[columnsfound] = k;
                     columnsfound++;
                  }
               }
            }
            if (columnsfound == 0) {
               /* The input could have been numbers */
               for (j = 0; j < MAXINPUTLEN; j++) {
                  ch = Tabcolnames.a[i*MAXINPUTLEN+j];
                  if (ch == ' ') break; else dcdstr[j] = ch;
               }
               dcdstr[j] = '\0';
               agreed = YES;
               r2 = dcdint_c( tofchar(dcdstr), numbers, &num, &r3 );
               agreed = (r3 >= 0);                      /* No conversion problem in 'dcdint' */
               if (agreed) {
                  for (j = 0; j < r2; j++) {
                     for (k = 0; k < currentcols; k++) {
                        if ( (strcmp(column[k].tablename,  tabname) == 0) &&
                             (column[k].colnr == numbers[j]) ) {
                           if (columnsfound < maxcols) {
                              columnarray[columnsfound] = k;
                              num--;
                           }
                           columnsfound++;
                        }
                     }
                  }
               } else {
                  if (r3 == -23) {
                     agreed = NO;
                     reject_c( Key, tofchar("Too many columns!") );
                  } else {
                     agreed = YES;
                     columnsfound = 0;
                  }
               }
            } /* End examination of numbers */
         } /* End for loop. All input fields are examined now */
         agreed = (columnsfound > 0);
         if (!agreed) reject_c( Key, tofchar("No columns found!") );
      } /* End if a table (name or number) was found */
      dfault1 = REQUEST;
   } while( !agreed );
   free( numbers );
   free( charbuf );
   strcpy( tablename, tabname );     /* return table name */
   if (columnsfound > maxcols) {
      sprintf( message, "Table returned % column(s) but found %d", maxcols, columnsfound );
      anyoutC( 1, message );
   }
   return( columnsfound );           /* return number of columns found */
}






FILE *getimportfile( char *filename )
/*-----------------------------------------------------*/
/* Open file for reading. Ask filename in GIPSY way    */
/* Check file for existence. Return file pointer       */
/* and the name of the given file.                     */
/* The function introduces the keyword IMPORTFILE=     */
/* The macro 'fmake' and the definitions for           */
/* YES and NO must be available.                       */
/*-----------------------------------------------------*/
{
#include    "stdio.h"
#include    "usertext.h"
#include    "userlog.h"
#include    "cancel.h"
#include    "reject.h"

#define    NAMELEN    80
#define    KEYLEN     20

   fchar     Filename;
   fint      request = 1;
   fint      dfault;
   fint      agreed;
   fint      n;
   fchar     Key, Mes;
   FILE     *fp;

   dfault = request;
   fmake( Filename, NAMELEN );
   fmake( Key, KEYLEN );
   fmake( Mes, NAMELEN );
   do {
      Key    = tofchar("IMPORTFILE=");
      Mes    = tofchar("Name of import ASCII file:     [no file]");
      n      = usertext_c( Filename,
                           &dfault,
                           Key,
                           Mes );
      if (n == 0) return NULL;
      (void) strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */
      fp = fopen(filename, "r");
      agreed = (fp != NULL);
      if (!agreed) {
         reject_c( Key, tofchar("Cannot open, try another!") );
      }
   } while (!agreed);
   cancel_c( Key );
   return( fp );                /* Return the file pointer */
}


static void initplot( void )
/*------------------------------------------------------------------*/
/* Initialize plot software. Set viewport and output dimensions.    */
/* If output device is a printer, ask user for line width.          */
/*------------------------------------------------------------------*/
{
   fint   unit;            /* Ignored by pgbeg, use unit=0. */
   fchar  Devspec;         /* Device specification. */
   fint   nxysub[2];       /* Number of subdivisions on 1 page. */
   float  width;           /* Width of output on paper */
   float  aspect;          /* Aspect ratio of output on paper */
   fint   nitems, dfault;
   fint   r1;
   fint   errlev = 4;      /* Set error level to fatal. */
   bool   pageoff;         /* Disable PGPLOT's NEXTPAGE keyword. */
   float  paper[2];
   float  xl, xr, yb, yt;  /* Edges of the viewport. */


   /* Begin PGPLOT, open output device. A return value of 1 indicates */
   /* successful completion. There are 4 arguments for PGBEG:         */
   /* UNIT, this argument is ignored by PGBEG (use zero).             */
   /* FILE, If this argument is a question mark PGBEG will prompt the */
   /*       user to supply a string.                                  */
   /* NXSUB,the number of subdivisions of the view surface in X.      */
   /* NYSUB,the number of subdivisions of the view surface in Y.      */

   nxysub[1] = nxysub[0] = 1;           /* Default no subdivisions in plot.*/
   nitems = 2;
   dfault = HIDDEN;
   r1 = userint_c( nxysub,
                   &nitems,
                   &dfault,
                   tofchar("PGMOSAIC="),
                   tofchar("View surface sub divisions in x,y:   [1,1]") );

   unit = 0;
   Devspec = tofchar("?");
   r1 = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r1 != 1) error_c( &errlev, tofchar("Cannot open output device") );

   /* No PGPLOT's NEXTPAGE= keyword */
   pageoff = toflog( 0 );
   pgask_c( &pageoff );

   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */
   nitems = 2; dfault = HIDDEN;
   paper[0] = 0.0; paper[1] = 1.0;
   r1 = userreal_c( paper,
                    &nitems,
                    &dfault,
                    tofchar("PGPAPER="),
                    tofchar("Give width(cm), aspect ratio: [calculated]") );
   if (r1 > 0) {
      /* If width = 0.0 then the program will select the largest view surface */
      width  = paper[0] / 2.54;      /* Convert width to inches. */
      aspect = paper[1];
      pgpap_c( &width, &aspect );
   }

   /* Set viewport */
   xl = 0.2; xr = 0.95;
   yb = 0.2; yt = 0.9;
   pgsvp_c( &xl, &xr, &yb, &yt );
}


static void drawframe( float *varXmin, float *varYmin,
                       float *varXmax, float *varYmax,
                       char *xtitle, char *ytitle, char *ttitle )
/*------------------------------------------------------------------*/
/* Draw box and labels. Take special care for the y labels and      */
/* title. Colors are defined globally. Xmin etc are the corners of  */
/* the box in world coordinates.                                    */
/*------------------------------------------------------------------*/
{
   float  charsize = 1.0;
   float  delta;
   fint   lwidth;
   fint   r1;
   fint   nitems;
   fint   dfault;
   float  drawbox[4];                         /* Corners of draw box. */
   fint   color;
   fint   font;
   fint   nxsub, nysub;
   float  xtick, ytick;
   fchar  Xtitle, Ytitle, Toptitle;
   char   message[160];
   float  Xmin, Ymin, Xmax, Ymax;




   pgpage_c();                         /* Advance to new page. */

   Xmin = *varXmin; Ymin = *varYmin;
   Xmax = *varXmax; Ymax = *varYmax;
   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Xmin -= delta; Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Ymin -= delta; Ymax += delta;
   drawbox[0] = Xmin; drawbox[1] = Ymin;      /* Get size from user input */
   drawbox[2] = Xmax; drawbox[3] = Ymax;
   nitems = 4; dfault = HIDDEN;
   (void) sprintf( message, "Corners of box Xl,Yl, Xh,Yh:  [%g,%g,%g,%g]", Xmin,Ymin,Xmax,Ymax );
   r1 = userreal_c( drawbox,
                    &nitems,
                    &dfault,
                    tofchar("PGBOX="),
                    tofchar( message ) );
   cancel_c( tofchar("PGBOX=") );
   Xmin = drawbox[0]; Ymin = drawbox[1];
   Xmax = drawbox[2]; Ymax = drawbox[3];
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );   /* Set the window */

   color = 1; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &color,
                   &nitems,
                   &dfault,
                   tofchar("PGCOLOR="),
                   tofchar("Give color 1..15:        [1]") );
   if (color > 15) color = 15;
   if (color < 1 ) color =  1;
   pgsci_c( &color );

   lwidth = 1; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &lwidth,
                   &nitems,
                   &dfault,
                   tofchar("PGWIDTH="),
                   tofchar("Give line width 1..21:        [1]") );
   if (lwidth > 21) lwidth = 21;
   if (lwidth < 1 ) lwidth =  1;
   pgslw_c( &lwidth );                  /* Set line width. */

   charsize = 1.0; nitems = 1; dfault = HIDDEN;
   r1 = userreal_c( &charsize,
                    &nitems,
                    &dfault,
                    tofchar("PGHEIGHT="),
                    tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );               /* Character height. */

   font = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &font,
                   &nitems,
                   &dfault,
                   tofchar("PGFONT="),
                   tofchar("Give font 1..4:        [2]") );
   if (font > 4) font = 4;
   if (font < 1) font = 1;
   pgscf_c( &font );                   /* Set font. */

   /* xtick is world coordinate interval between major tick marks    */
   /* on X axis. If xtick=0.0, the interval is chosen by PGBOX, so   */
   /* that there will be at least 3 major tick marks along the axis. */
   /* nxsub is the number of subintervals to divide the major        */
   /* coordinate interval into. If xtick=0.0 or nxsub=0, the number  */
   /* is chosen by PGBOX.                                            */
   /* BCNSTV :                                                       */
   /* B: draw bottom (X) or left (Y) edge of frame.                  */
   /* C: draw top (X) or right (Y) edge of frame.                    */
   /* N: write Numeric labels in the conventional location below     */
   /*    the viewport (X) or to the left of the viewport (Y).        */
   /* S: draw minor tick marks (Subticks).                           */
   /* T: draw major Tick marks at the major coordinate interval.     */
   /* V: orient numeric labels Vertically. This is only applicable   */
   /*    to Y.                                                       */
   xtick = ytick = 0.0;
   nxsub = nysub = 0;
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
            tofchar("BCNSTV"), &ytick, &nysub );

   /* Create titles */

   fmake( Xtitle, 80 ); fmake( Ytitle, 80 ); fmake( Toptitle, 80);
   Xtitle = tofchar(xtitle);
   Ytitle = tofchar(ytitle);
   Toptitle = tofchar(ttitle);
   pglab_c( Xtitle, Ytitle, Toptitle );

   /* Return (adjusted) size of plot frame */
   *varXmin = Xmin; *varYmin = Ymin;
   *varXmax = Xmax; *varYmax = Ymax;
}



static int inlist( int cols, fchar Setin, fchar Tablename,
                   fchar Columnname, fint subset )
/*---------------------------------------------------*/
/* Does the table in Setin with name Tablename,      */
/* columnname already exist, return the columnnumber */
/* else return a non existent column number.         */
/*---------------------------------------------------*/
{
   int    k;
   bool   exist;


   for (k = 0; k < cols; k++) {
      int    len1, len2, len3;
      len1 = MYMIN( strlen(column[k].tablename),  Tablename.l  );
      len2 = MYMIN( strlen(column[k].columnname), Columnname.l );
      len3 = MYMIN( strlen(column[k].setname),    Setin.l      );
      exist = ( (strncmp( column[k].tablename,  Tablename.a,  len1 ) == 0) &&
                (strncmp( column[k].columnname, Columnname.a, len2 ) == 0) &&
                (strncmp( column[k].setname,    Setin.a,      len3 ) == 0) &&
                (column[k].subset == subset) );
      if (exist) return(k);
   }
   return(-1);
}



static void getsetname( fchar Setname, fint *subsets, fint *numsubs  )
/*-------------------------------------------------------------------------*/
/* Get set name, and check existence. Return Fortran type string.          */
/*-------------------------------------------------------------------------*/
{
   fint     nitems = 1;
   fint     r1;
   fint     nsubs;
   fint     maxsubs = MAXSUBSETS;
   fint     dfault = NONE;
   fint     showdev = 8;
   fint     axnum[MAXAXES];
   fint     axcount[MAXAXES];
   fint     maxaxes = MAXAXES;  /* Max num. of axes the program can deal with.*/
   fint     class = 1;          /* Class 1 is for applications which repeat operations */
   fint     subdim = 0;


   Key    = tofchar("INSET=");
   Mes    = tofchar("Give set (, subsets):");
   nsubs  = gdsinp_c( Setname,    /* Name of input set. */
                      subsets,    /* Array containing subsets coordinate words. */
                      &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                      &dfault,    /* Default code as is USERxxx. */
                      Key,        /* Keyword prompt. */
                      Mes,        /* Keyword message for the user. */
                      &showdev,   /* Device number (as in ANYOUT). */
                      axnum,      /* Array of size 'maxaxes' containing the axes numbers. */
                                  /* The first elements (upto the dimension of the subset) */
                                  /* contain the axes numbers of the subset, */
                                  /* the other ones contain the axes numbers */
                                  /* outside the subset ordered according to the */
                                  /* specification by the user. */
                      axcount,    /* Number of grids on axes in 'axnum' */
                      &maxaxes,   /* Max. number of axes. */
                                  /* the operation for each subset. */
                      &class,     /* Class 1 is for applications which repeat operations */
                      &subdim );  /* Dimensionality of the subsets for class 1 */

   if (subsets[0] == 0) {    /* Top level */
      bool   top;
      nitems  = 1;
      top = toflog( YES );
      Key = tofchar("TOP=");
      Mes = tofchar("Tables on top level only?     [Y]/N(=All levels)");
      dfault = HIDDEN;
      r1     = userlog_c( &top, &nitems, &dfault, Key, Mes );
      top    = tobool( top );
      if (!top) nsubs = 0;
   }
   *numsubs = nsubs;
}


static void defformat( char *type, char *formatstr )
{
   int   nchar;
   int   j;


   if (strstr(type, "REAL") != NULL) {
      (void) strcpy( formatstr, "ffffff.ffff" );
   }
   if (strstr(type, "DBLE") != NULL) {
      (void) strcpy( formatstr, "ffffffff.ffff" );
   }
   if (strstr(type, "INT") != NULL) {
      (void) strcpy( formatstr, "fffff" );
   }
   if (strstr(type, "CHAR") != NULL) {
      /* Get width of the character strings: syntax is "CHARnnnn" */
      if (!type[4]) {
         nchar = 1;
      } else {
         nchar = MYMIN( NORMLEN-1, atoi(&type[4]) );
      }
      (void) strcpy( formatstr, "f" );
      for (j = 1; j < nchar; j++) {
         (void) strcat( formatstr, "f" );
      }
   }
   if (strstr(type, "LOG") != NULL) {
      (void) strcpy( formatstr, "fffff" );
   }
}


static void getlevel( fchar Setin, fint subset, char *levelstr )
/*--------------------------------------------------*/
/* Create string with subset level.                 */
/*--------------------------------------------------*/
{
   fint   setdim;
   fint   zero = 0;
   int    n;
   fint   iax;
   fint   err;
   fint   grid;


   setdim = gdsc_ndims_c( Setin, &zero );
   (void) strcpy( levelstr, "(" );
   for (n = 0; n < setdim; n++ ) {
      iax = n + 1;
      err = 0;
      grid = gdsc_grid_c( Setin, &iax, &subset, &err );
      if (err >= 0) {
         (void) sprintf( levelstr, "%.*s%d ", strlen(levelstr), levelstr, (int) grid );
      } else {
         (void) sprintf( levelstr, "%.*s* ", strlen(levelstr), levelstr );
      }
   }
   levelstr[strlen(levelstr)-1] = ')';
}


static void errmsg( fint errnr )
{
   int   scr = 8;
   switch ((int)errnr) {
      case -66 :
         anyoutC( scr, "TABLE: Descriptor file not present" );
      break;
      case -67 :
         anyoutC( scr, "TABLE: Illegal data type or wrong data type in column" );
      break;
      case -68 :
         anyoutC( scr, "TABLE: Reading past end of information" );
      break;
      case -69 :
         anyoutC( scr, "TABLE: Attempt to skip rows in writing" );
      break;
      case -70 :
         anyoutC( scr, "TABLE: End of information" );
      break;
      case -71 :
         anyoutC( scr, "TABLE: Number of items in list too small" );
      break;
      default  :
         anyoutC( scr, "TABLE: Unkown GDS error" );
      break;
   }
}



static void extendlist( int colnr, fchar Setin, fchar Tablename,
                        fchar Columnname, fchar Comment, fchar Type,
                        fchar Units, fint subset, fint arraylen )
/*----------------------------------------------------------------------*/
/* The structure 'column' must be updated everytime a new column is     */
/* created or deleted. If a column is created, use 'extendlist' and     */
/* display the new list.                                                */
/*----------------------------------------------------------------------*/
{
   /* To start again in the same session: */
   if (colnr == 0) {
      if (column != NULL) {
         free( column );
      }
      column = (columnstruct *) calloc( INITCOLNUM, sizeof(columnstruct) );
   } else {
      if (!(colnr % INITCOLNUM) ) {
         int s;
         s = ((colnr+INITCOLNUM)/INITCOLNUM) * INITCOLNUM;
         column = realloc( column, s * sizeof(columnstruct) );
         if (column == NULL) {
            anyoutC( 1, "Cannot extend list with columns!" );
            return;
         }
      }
   }

   /* Now 'char2str' copies exactly 'nelc_c' characters */
   (void) char2str( Setin,      column[colnr].setname,    1 + (int) nelc_c(Setin) );
   (void) char2str( Tablename,  column[colnr].tablename,  1 + (int) nelc_c(Tablename) );
   (void) char2str( Columnname, column[colnr].columnname, 1 + (int) nelc_c(Columnname) );
   (void) char2str( Comment,    column[colnr].comment,    1 + (int) nelc_c(Comment) );
   (void) char2str( Type,       column[colnr].type,       1 + (int) nelc_c(Type) );
   (void) char2str( Units,      column[colnr].units,      1 + (int) nelc_c(Units) );

   column[colnr].subset = (int) subset;
   column[colnr].size   = (int) arraylen;
   getlevel(  Setin,  subset, column[colnr].level );
   defformat( Type.a, column[colnr].format );
}




static void fillcolstruc( fchar Setin, fint *subsets, int numsubs,
                          int *cols, int *tabs )
/*---------------------------------------------------------------*/
/* List all tables present in a GDS descriptor file              */
/*---------------------------------------------------------------*/
{
   fchar          Tablenames;
   fint           *tablesubsets = NULL;
   fint           tabsfound;
   int            tabnr;
   int            i, n;
   fint           r1;
   int            columnnumber;
   bool           store;
   fint           maxtables  = INITCOLNUM;    /* This number can increase! */
   fint           maxcolumns = INITCOLNUM;    /* This number can increase! */


   tabsfound = 0;
   do {
      /* Create space for all table names */
      finit( Tablenames, maxtables*(SHORTLEN+1) );
      clearstr( Tablenames );
      Tablenames.l = SHORTLEN;
      tablesubsets = (fint *) calloc( (int) maxtables, sizeof(fint) );
      if (tablesubsets == NULL) {
         anyoutC( 1, "Cannot allocate memory to store subsets!" );
         return;
      }
      r1 = 0;
      /*---------------------------------------------------*/
      /* For this set, get all table names and the subsets */
      /* where to find them.                               */
      /*---------------------------------------------------*/
      gdsa_tablis_c( Setin,                   /* Set name */
                     tablesubsets,            /* List of subsets where tables were found */
                     Tablenames,              /* List of GDS tables present */
                     &maxtables,              /* Size of Tnames and Tsubsets */
                     &tabsfound,              /* Number of tables found */
                     &r1 );                   /* Error return code */
      agreed = (r1 >= 0);
      if (r1 == -71) {
         maxtables += maxtables / 2;
         free( Tablenames.a );
         free( tablesubsets );
      }
   } while (!agreed);
   *tabs += tabsfound;

   (void) sprintf( message,
           "Number of tables found in %.*s is %d",
            nelc_c(Setin), Setin.a,
            tabsfound );
   anyoutC( 16, message );

   columnnumber = *cols;
   for (tabnr = 0; tabnr < tabsfound; tabnr++) {
      /* Create space for all column names for this table. */
      fchar  Tabname;
      fchar  Columnnames;
      fint   colsfound;
      fint   collength;
      fchar  Colname;
      fchar  Datatype;
      fchar  Units;
      fchar  Comment;

      fmake( Tabname, SHORTLEN );
      do {
         finit( Columnnames, maxcolumns*(SHORTLEN+1) );
         clearstr( Columnnames );
         clearstr( Tabname );
         Columnnames.l = SHORTLEN;    /* 'maxcolumns' small strings */
         r1 = 0;
         strncpy(Tabname.a, &Tablenames.a[(SHORTLEN)*tabnr], SHORTLEN);
         Tabname.a[SHORTLEN] = '\0';
         Tabname.l = SHORTLEN;
         gdsa_tabinq_c( Setin,                  /* Name of GDS set */
                        &tablesubsets[tabnr],   /* Level */
                        Tabname,                /* Name of the Table */
                        Columnnames,            /* Name of GDS columns found */
                        &maxcolumns,
                        &colsfound,             /* Number of columns found */
                        &r1 );
         agreed = (r1 >= 0);
         if (r1 == -71) {
            maxcolumns += maxcolumns / 2;
            free( Columnnames.a );
         }
      } while (!agreed);
      fmake( Colname,  SHORTLEN );
      fmake( Datatype, VARLEN );
      fmake( Comment,  VARLEN );
      fmake( Units,    VARLEN );


      for (i = 0; i < colsfound; i++) {
         clearstr( Colname );
         clearstr( Datatype );
         clearstr( Comment );
         clearstr( Units );
         strncpy( Colname.a, &Columnnames.a[SHORTLEN*i], SHORTLEN );
         Colname.a[SHORTLEN] = '\0';
         Colname.l = SHORTLEN;
         r1 = 0;
         gdsa_colinq_c( Setin,
                        &tablesubsets[tabnr],      /* Subset where table is to be examined. */
                        Tabname,                   /* Name of GDS table. */
                        Colname,                   /* Name of GDS column. */
                        Datatype,                  /* Data type of column. */
                        Comment,                   /* Comment for this column. */
                        Units,                     /* Units of data in column. */
                        &collength,                /* Number of items in column. */
                        &r1 );
         if (r1 < 0) {
            Datatype = tofchar("?");
            Units    = tofchar("?");
            Comment  = tofchar("?");
         } else {
               /* char           tabmes[MAXTABBUF];
                  sprintf( tabmes, "%-4d %-8.8s  %-8.8s  %-8.8s  %-8.8s  %5d  %-.*s",
                     columnnumber,
                     Tabname.a,
                     Colname.a,
                     Datatype.a,
                     Units.a,
                     collength,
                     MYMIN( nelc_c( Comment ), (MAXTABBUF-60) ),
                     Comment.a );
                anyoutC( 3, tabmes ); */
            /* Store these properties in the table struct. */
            store = (numsubs == 0);
            if (!store) {
               for (n = 0; n < numsubs; n++) {
                  if (tablesubsets[tabnr] == subsets[n]) {
                     store = YES;
                     break;
                  }
               }
            }
            if (store) {
               extendlist( columnnumber, Setin, Tabname, Colname, Comment, Datatype,
                           Units, (int) tablesubsets[tabnr], collength );
               columnnumber++;
            }
         }
      }
   }
   *cols = columnnumber;
}




int compare( char *s1, char *s2 )
/*-----------------------------------------------------------*/
/* Compare second field of two structures. If they are equal */
/* (i.e. equal table names) then compare the third field     */
/* (the column name). This function is called by the qsort   */
/* function.                                                 */
/*-----------------------------------------------------------*/
{
   int res;


   /* Compare second field */
   res = strcmp( &s1[LONGLEN], &s2[LONGLEN] );
   if (res == 0) {
      /* then compare also third field */
      res = strcmp( &s1[LONGLEN+SHORTLEN], &s2[LONGLEN+SHORTLEN] );
   }
   return(res);
}



static void sortandprint( int cols )
/*---------------------------------------------*/
/* The struct 'column' is global. Sort this    */
/* struct on table and column name and print.  */
/*---------------------------------------------*/
{
   int            i, j, k;
   char           tabmes[MAXTABBUF];
   int            clen;
   int            r;
   int            inserted = NO;
   int            tabc, colc;
   char           border[MAXTABBUF];
   int            *key = NULL;       /* Key array for sorting purpose */


   if (cols == 0) {
      anyoutC( 1, "No columns found" );
      return;
   }
   key = (int *) calloc( cols, sizeof(int) );
   if (key == NULL) {
      anyoutC( 1, "Cannot allocate memory to sort columns!" );
      return;
   }

   memset( border, '=', 132 );
   anyoutC( 3, border );
   (void) sprintf( tabmes, "%-3.3s %-3.3s %-8.8s %-8.8s %-8.8s %-8.8s %-5.5s  %-10s         Comment",
            "tab",
            "col",
            "Table",
            "Column",
            "Type",
            "Units",
            "Size",
            "Level  Set" );
   anyoutC( 3, tabmes );
   anyoutC( 3, border );

/*   qsort( column, cols, sizeof(column[0]), (int(*)())compare );  */


   i = 0; key[0] = i;

   for (i = 1; i < cols; i++) {
      for (j = 0; j < i; j++) {
         inserted = NO;
         r = strcmp( column[i].setname,   column[key[j]].setname );
         if (r == 0) {
            r = strncmp( column[i].tablename, column[key[j]].tablename, SHORTLEN );
         }
         if (r == 0) {
            r = (column[i].subset - column[key[j]].subset);
         }
         if (r == 0) {
            r = strncmp( column[i].columnname, column[key[j]].columnname, SHORTLEN );
         }
         if (r < 0) {
            /* Insert this entry, i.e. move from j to i and insert j */
            for (k = i-1; k >= j; k--) {
               key[k+1] = key[k];
            }
            key[j] = i;
            inserted = YES;
            break;            /* Insert only once */
         }
      }
      if (!inserted) {
         key[i] = i;
      }
   }

   tabc = 1; colc = 1;
   for (i = 0; i < cols; i++) {
      k = key[i];
      if (i > 0) {
         colc++;
         if ( (strcmp( column[key[i]].tablename, column[key[i-1]].tablename) != 0) ||
              (strcmp( column[key[i]].setname,   column[key[i-1]].setname) != 0)   ||
              (column[key[i]].subset != column[key[i-1]].subset)  ) {
            tabc++; colc = 1;
         }
      }
      column[k].tabnr = tabc;
      column[k].colnr = colc;
      clen = strlen( column[k].comment );
      (void) sprintf( tabmes, "%-3d %-3d %-8.8s %-8.8s %-8.8s %-8.8s %-5d  %-.*s  %-.*s",
               tabc,
               colc,
               column[k].tablename,
               column[k].columnname,
               column[k].type,
               column[k].units,
               column[k].size,
               strlen(column[k].level),
               column[k].level,
               strlen(column[k].setname),
               column[k].setname );
      clen = MYMIN( clen, (MAXTABBUF-strlen(tabmes)-3));
      if (clen < 0) clen = 0;
      (void) sprintf( tabmes, "%.*s  %.*s",
               strlen(tabmes), tabmes,
               clen, column[k].comment );
      anyoutC( 3, tabmes );
   }
   anyoutC( 3, border );
}



static fint *column2fint( int colnr )
/*------------------------------------------------------------*/
/* Read data of a column into array. Return address of array. */
/* Return NULL if 1) no memory could be allocated, 2) The     */
/* column is of different type or 3) an error occurred in the */
/* read routine.                                              */
/*------------------------------------------------------------*/
{
   fint   size;
   int    k;
   fint   readstart;
   fint   r1;
   fint   subset;
   fint   *fintarray = NULL;


   k = colnr;
   size = (fint) column[k].size;
   if (strstr(column[k].type, "INT") != NULL) {
      fintarray = (fint *) calloc( (int) column[k].size, sizeof(fint) );
      if (fintarray == NULL) {
         anyoutC( 1, "Cannot allocate memory to display column" );
         return(NULL);
      } else {
         readstart = 1;
         r1 = 0;
         subset = (fint) column[k].subset;
         /*-----------------------------------------*/
         /* Read items from a column in a GDS table */
         /*-----------------------------------------*/
         gdsa_rcint_c( tofchar(column[k].setname),   /* Name of GDS set. */
                       &subset,                      /* Subset where table is to be read. */
                       tofchar(column[k].tablename), /* Name of GDS table. */
                       tofchar(column[k].columnname),/* Name of GDS column. */
                       fintarray,                    /* Array containing the data to be written */
                       &readstart,                   /* Row number where to start */
                       &size,                        /* Number of rows to read. */
                       &r1 );                        /* Error return code. */

         if (r1 < 0) {
            errmsg( r1 );
            return(NULL);
         }
      }
   } else {
      anyoutC( 1, "Column not of type INT" );
      return(NULL);
   }
   return(fintarray);
}


static float *column2real( int colnr )
/*------------------------------------------------------------*/
/* Read data of a column into array. Return address of array. */
/* Return NULL if 1) no memory could be allocated, 2) The     */
/* column is of different type or 3) an error occurred in the */
/* read routine.                                              */
/*------------------------------------------------------------*/
{
   fint   size;
   int    k;
   fint   readstart;
   fint   r1;
   fint   subset;
   float  *realarray = NULL;


   k = colnr;
   size = (fint) column[k].size;
   if (strstr(column[k].type, "REAL") != NULL) {
      realarray = (float *) calloc( (int) column[k].size, sizeof(float) );
      if (realarray == NULL) {
         anyoutC( 1, "Cannot allocate memory to display column" );
         return(NULL);
      } else {
         readstart = 1;
         r1 = 0;
         subset = (fint) column[k].subset;
         /*-----------------------------------------*/
         /* Read items from a column in a GDS table */
         /*-----------------------------------------*/
         gdsa_rcreal_c( tofchar(column[k].setname),   /* Name of GDS set. */
                        &subset,                      /* Subset where table is to be read. */
                        tofchar(column[k].tablename), /* Name of GDS table. */
                        tofchar(column[k].columnname),/* Name of GDS column. */
                        realarray,                    /* Array containing the data to be written */
                        &readstart,                   /* Row number where to start */
                        &size,                        /* Number of rows to read. */
                        &r1 );                        /* Error return code. */

         if (r1 < 0) {
            errmsg( r1 );
            return(NULL);
         }
      }
   } else {
      anyoutC( 1, "Column not of type REAL" );
      return(NULL);
   }
   return(realarray);
}


static double *column2double( int colnr )
/*------------------------------------------------------------*/
/* Read data of a column into array. Return address of array. */
/* Return NULL if 1) no memory could be allocated, 2) The     */
/* column is of different type or 3) an error occurred in the */
/* read routine.                                              */
/*------------------------------------------------------------*/
{
   fint   size;
   int    k;
   fint   readstart;
   fint   r1;
   fint   subset;
   double *doublearray = NULL;


   k = colnr;
   size = (fint) column[k].size;
   if (strstr(column[k].type, "DBLE") != NULL) {
      doublearray = (double *) calloc( (int) column[k].size, sizeof(double) );
      if (doublearray == NULL) {
         anyoutC( 1, "Cannot allocate memory to display column" );
         return(NULL);
      } else {
         readstart = 1;
         r1 = 0;
         subset = (fint) column[k].subset;
         /*-----------------------------------------*/
         /* Read items from a column in a GDS table */
         /*-----------------------------------------*/
         gdsa_rcdble_c( tofchar(column[k].setname),   /* Name of GDS set. */
                        &subset,                      /* Subset where table is to be read. */
                        tofchar(column[k].tablename), /* Name of GDS table. */
                        tofchar(column[k].columnname),/* Name of GDS column. */
                        doublearray,                  /* Array containing the data to be written */
                        &readstart,                   /* Row number where to start */
                        &size,                        /* Number of rows to read. */
                        &r1 );                        /* Error return code. */

         if (r1 < 0) {
            errmsg( r1 );
            return(NULL);
         }
      }
   } else {
      anyoutC( 1, "Column not of type DBLE" );
      return(NULL);
   }
   return(doublearray);
}


static bool *column2log( int colnr )
/*------------------------------------------------------------*/
/* Read data of a column into array. Return address of array. */
/* Return NULL if 1) no memory could be allocated, 2) The     */
/* column is of different type or 3) an error occurred in the */
/* read routine.                                              */
/*------------------------------------------------------------*/
{
   fint   size;
   int    k;
   fint   readstart;
   fint   r1;
   fint   subset;
   bool   *boolarray = NULL;


   k = colnr;
   size = (fint) column[k].size;
   if (strstr(column[k].type, "LOG") != NULL) {
      boolarray = (bool *) calloc( (int) column[k].size, sizeof(bool) );
      if (boolarray == NULL) {
         anyoutC( 1, "Cannot allocate memory to display column" );
         return(NULL);
      } else {
         readstart = 1;
         r1 = 0;
         subset = (fint) column[k].subset;
         /*-----------------------------------------*/
         /* Read items from a column in a GDS table */
         /*-----------------------------------------*/
         gdsa_rclog_c( tofchar(column[k].setname),   /* Name of GDS set. */
                       &subset,                      /* Subset where table is to be read. */
                       tofchar(column[k].tablename), /* Name of GDS table. */
                       tofchar(column[k].columnname),/* Name of GDS column. */
                       boolarray,                    /* Array containing the data to be written */
                       &readstart,                   /* Row number where to start */
                       &size,                        /* Number of rows to read. */
                       &r1 );                        /* Error return code. */

         if (r1 < 0) {
            errmsg( r1 );
            return(NULL);
         }
      }
   } else {
      anyoutC( 1, "Column not of type LOG" );
      return(NULL);
   }
   return(boolarray);
}



static char *column2char( int colnr, int *charlen )
/*------------------------------------------------------------*/
/* Read data of a column into array. Return address of array. */
/* Return NULL if 1) no memory could be allocated, 2) The     */
/* column is of different type or 3) an error occurred in the */
/* read routine.                                              */
/*------------------------------------------------------------*/
{
   fint   size;
   int    k;
   fint   readstart;
   fint   r1;
   fint   subset;
   char   *chararray = NULL;
   int    nchar;                               /* Field width of column */
   fchar  Fchararray;
   int    len;
   int    i;


   k = colnr;
   size = (fint) column[k].size;
   if (strncmp( "CHAR", column[k].type, 4) == 0) {
      /* Get width of the character strings: syntax is "CHARnnnn" */
      if (!column[k].type[4]) {
         nchar = 1;
      } else {
         nchar = atoi( &column[k].type[4] );
      }
      if (nchar > 132) nchar = 132;            /* n cannot be greater than 132. */
      len = (int) size*nchar + 1;
      chararray = (char *) calloc( len, sizeof(char) );
      if (chararray == NULL) {
         anyoutC( 1, "Cannot allocate memory to display column" );
         return(NULL);
      } else {
         for (i = 0; i < len; i++) chararray[i] = ' ';
         readstart = 1;
         Fchararray.a = chararray;
         Fchararray.l = nchar;
         r1 = 0;
         subset = (fint) (column[k].subset);
         /*-----------------------------------------*/
         /* Read items from a column in a GDS table */
         /*-----------------------------------------*/
         gdsa_rcchar_c( tofchar(column[k].setname),
                        &subset,
                        tofchar(column[k].tablename),
                        tofchar(column[k].columnname),
                        Fchararray,
                        &readstart,
                        &size,
                        &r1 );
         if (r1 < 0) {
            errmsg( r1 );
            return(NULL);
         }
         /* Make sure these are C compatible strings.
         for (i = 1; i <= size; i++) chararray[i*nchar-1] = '\0'; */
      }
   } else {
      anyoutC( 1, "Column not of type CHAR" );
      return(NULL);
   }
   *charlen = Fchararray.l;
   return(Fchararray.a);
}


static int fillrealarray( int colnr, floatptr *arrayptr )
/*---------------------------------------------------------*/
/* Create room for an array to hold data of column 'colnr' */
/* Return a pointer to the the filled column and the size  */
/* of the column. The column is filled with floats only.   */
/*---------------------------------------------------------*/
{
   int      sizeX;
   floatptr Xarray = NULL;
   int      j;


   sizeX = (fint) column[colnr].size;
   if (strstr(column[colnr].type, "REAL") != NULL) {
      Xarray = column2real(colnr);
   }
   if (strstr(column[colnr].type, "INT") != NULL) {
      fint    *fintarray = NULL;
      Xarray = (float *) calloc( (int) sizeX, sizeof(float) );
      if (Xarray == NULL) {
         anyoutC( 1, "Cannot allocate memory to display column" );
      }
      fintarray = column2fint(colnr);
      if (fintarray == NULL) {
         anyoutC( 1, "Cannot allocate memory to display column");
         return(0);
      }
      for (j = 0; j < (int) sizeX; j++) {
         Xarray[j] = (float) fintarray[j];
      }
      free( fintarray );
   }
   if (strstr(column[colnr].type, "DBLE") != NULL) {
      double   *doublearray = NULL;
      Xarray = (float *) calloc( (int) sizeX, sizeof(float) );
      if (Xarray == NULL) {
         anyoutC( 1, "Cannot allocate memory to display column" );
      }
      doublearray = column2double(colnr);
      if (doublearray == NULL) {
         anyoutC( 1, "Cannot allocate memory to display column" );
         return(0);
      }
      for (j = 0; j < (int) sizeX; j++) {
         Xarray[j] = (float) doublearray[j];
      }
      free( doublearray );
   }
   if (!strncmp( "CHAR", column[colnr].type, 4)) {
      anyoutC( 1, "Cannot transform character data!" );
      Xarray = NULL;
   }
   if (strstr(column[colnr].type, "LOG") != NULL) {
      anyoutC( 1, "Cannot transform logical data!" );
      Xarray = NULL;
   }
   *arrayptr = Xarray;
   if (Xarray == NULL) {
      return(0);
   }
   return(sizeX);
}


static bool hmsdms(char *format, char *mode, int *len)
/*-------------------------------------------------------*/
/* Syntax for format is HMSn or DMSn where n is a number */
/* If "HMS" or "DMS" is in the format, try to convert n  */
/* Return 0 or n if n could be converted.                */
/*-------------------------------------------------------*/
{
   int   i, j;
   char  number[4];

   for (i = 0; i < strlen( format ); i++) {     /* Format to upper case */
      format[i] = toupper( format[i] );
   }
   *len = 0;
   if (strstr( format, mode ) != NULL) {        /* Is it HMS or DMS? */
      if (strlen( format ) > 3) {               /* Is a number attached? */
         i = 3;
         j = 0;
         while (isdigit(format[i])) number[j++] = format[i++];
         number[j] = '\0';
         if (j > 0) {
            *len = atoi(number);
         }
      }
      return(YES);
   }
   return(NO);
}



static int getformat( int formatnr, int colnr )
/*--------------------------------------------------------------*/
/* Ask user to give 'format image' and store this string in the */
/* column structure.                                            */
/*--------------------------------------------------------------*/
{
   fint    r1;
   fchar   Formatstr;
   fint    dfault, nitems;
   char    messbuf1[20];
   char    messbuf2[256];
   int     prec;


   fmake( Formatstr, NORMLEN )
   (void) sprintf( messbuf1, "FORMAT%d=", formatnr );
   Key = tofchar( messbuf1 );
   (void) sprintf( messbuf2, "Give format for %.*s type:   [%.*s]",
            strlen(column[colnr].type), column[colnr].type,
            strlen(column[colnr].format), column[colnr].format );
   Mes = tofchar( messbuf2 );
   dfault = REQUEST;
   nitems = 1;
   (void) str2char( column[colnr].format, Formatstr );
   r1 = userchar_c( Formatstr, &nitems, &dfault, Key, Mes );
   cancel_c( Key );
   FtoCstr( Formatstr, column[colnr].format, NORMLEN );
   if (hmsdms( column[colnr].format, "HMS", &prec )) return(HMSLEN+prec);
   if (hmsdms( column[colnr].format, "DMS", &prec )) return(DMSLEN+prec);
   return( (int) nelc_c( Formatstr ) );
}




static int getchoice( void )
/*------------------------------------------------------*/
/* Display menu and ask option,                         */
/*------------------------------------------------------*/
{
#define        MENUITEMS   14
   fint        choice;
   fint        nitems;
   fint        dfault;
   fint        r1;
   int         agreed;
   static int  first = YES;
   int         scr = 8;


   if (first) {
      anyoutC( scr, " ");
      anyoutC( scr, "                     =============== TABLE MENU ==============");
      anyoutC( scr, " ");
      anyoutC( scr, "                     1) Exit program");
      anyoutC( scr, "                     2) Give new set or append one");
      anyoutC( scr, "                     3) Set filter column");
      anyoutC( scr, "                     4) Create (and fill/import) new column");
      anyoutC( scr, "                     5) View, export (formatted) table data" );
      anyoutC( scr, "                     6) Plot column(s)");
      anyoutC( scr, "                     7) Next keyword options");
      anyoutC( scr, "                     8) Fill column using expressions with columns as variables");
      anyoutC( scr, "                     9) Elementary statistics on column data");
      anyoutC( scr, "                    10) Correlation between two columns");
      anyoutC( scr, "                    11) Sort a column");
      anyoutC( scr, "                    12) Edit (and/or append to) a column");
      anyoutC( scr, "                    13) Delete a column");
      anyoutC( scr, "                    14) Copy contents of column to another column");
      anyoutC( scr, " ");
   }


   first  = NO;
   nitems = 1;
   dfault = REQUEST;
   choice = 1;
   Key    = tofchar("OPTION=");
   do {
      Mes    = tofchar("[1]=Exit,2=Set,3=Filter,4=Create,5=View,6=Plt,7=NEXT");
      r1     = userint_c( &choice, &nitems, &dfault, Key, Mes );
      agreed = ( (choice >= 1) && (choice <= MENUITEMS) );
      if (!agreed) reject_c( Key, tofchar("Wrong option!") );
      if (choice == 7) {
         Mes = tofchar("8=Clc,9=Stat,10=Cor,11=Srt,12=Edt,13=Del,14=Cpy [back]");
         cancel_c(tofchar("OPTION="));
         r1 = userint_c( &choice, &nitems, &dfault, Key, Mes );
         agreed = ( (choice >= 1) && (choice <= MENUITEMS) );
         if (!agreed) reject_c( Key, tofchar("Wrong option!") );
         if (r1 == 0) {
            cancel_c(tofchar("OPTION="));
            agreed = NO;
         }
      }
   } while (!agreed);
   cancel_c( Key );
   return(choice);
}



static void getheader( int formatnr, int colnr )
/*--------------------------------------------------------------*/
/* Ask user to give a name and units as header of a column. The */
/* new values are stored in the column structure.               */
/*--------------------------------------------------------------*/
{
   fint    r1;
   fchar   Namestr, Unitstr;
   fint    dfault, nitems;
   char    messbuf1[20];
   char    messbuf2[60];


   fmake( Namestr, NORMLEN );
   (void) sprintf( messbuf1, "COLNAME%d=", formatnr );
   Key = tofchar(messbuf1);
   (void) sprintf( messbuf2, "Give name of column:   [%s]",
                   column[colnr].columnname );
   Mes = tofchar( messbuf2 );
   dfault = REQUEST;
   nitems = 1;
   (void) str2char( column[colnr].columnname, Namestr );
   r1 = usertext_c( Namestr, &dfault, Key, Mes );
   cancel_c( Key );
   FtoCstr( Namestr, column[colnr].header, NORMLEN );

   fmake( Unitstr, NORMLEN );
   (void) sprintf( messbuf1, "COLUNITS%d=", formatnr );
   Key = tofchar(messbuf1);
   (void) sprintf( messbuf2, "Give units for column:    [%s]",
                   column[colnr].units );
   Mes = tofchar( messbuf2 );
   dfault = REQUEST;
   nitems = 1;
   (void) str2char( column[colnr].units, Unitstr );
   r1 = usertext_c( Unitstr, &dfault, Key, Mes );
   cancel_c( Key );
   FtoCstr( Unitstr, column[colnr].units, NORMLEN );
}



static void addtostr( char *str1, char *str2, int fieldwidth, int extraspaces )
/*----------------------------------------------------------------------*/
/* Append str2 to str1 and extend str1 with spaces. The total number of */
/* spaces is the number spaces needed to fill the field width and the   */
/* number of spaces to add as white space between columns.              */
/*----------------------------------------------------------------------*/
{
   int   n, n1, n2;

   n1 = strlen( str1 );
   n2 = strlen( str2 );
   (void) strcat( str1, str2 );
   for (n = n2; n < (fieldwidth+extraspaces); n++) str1[n1+n] = ' ';
   str1[n1+n] = '\0';
}



static int convhmsdms( char *format, double *dvalue,
                       fchar Formatstr, char *mode, fint prec)
/*------------------------------------------------------------------*/
/* The function determines if a conversion is needed to hms or dms  */
/* (depending on 'mode' and mode is "HMS" or "DMS"). The number     */
/* conversion is done by the functions 'hms' and 'dms'. The results */
/* have fixed lengths and are adjusted to the right. The function   */
/* returns the length of the formatted string.                      */
/*------------------------------------------------------------------*/
{
   fint    zero = 0;
   int     len, left, maxlen;
   char    dummy[20];


   if (strstr( mode, "HMS" ) != NULL) {
      hms_c( dvalue, Formatstr, NULL, &prec, &zero );
      maxlen = HMSLEN + prec;
   } else {
       if (strstr( mode , "DMS" ) != NULL) {
         dms_c( dvalue, Formatstr, NULL, &prec, &zero );
         maxlen = DMSLEN + prec;
      } else {
         return( 0 );
      }
   }
   len = nelc_c(Formatstr);
   left = maxlen - len;
   if (left > 0) {
      memset( dummy, ' ' , left );
      strncpy( &dummy[left], Formatstr.a, (maxlen-left) );
      dummy[maxlen] = '\0';
      (void) str2char( dummy, Formatstr );
   }
   return( maxlen );
}





static void displaycol( fint currentcols )
/*-----------------------------------------------*/
/* Ask user in loop the column(s) to be used in  */
/* the output. For each column a format is       */
/* asked. Each format has its own keyword, i.e.  */
/* FORMAT1=, FORMAT2=, etc. A table with columns */
/* is send to screen, printer or ASCII file.     */
/* The array 'colarray' contains the indices     */
/* of the columns. There are 'ncols' columns     */
/* to print.                                     */
/* A complete column is read and  stored in a    */
/* character buffer. The next column will be     */
/* read and appended at the end of each existing */
/* line. The advantage is that you can read the  */
/* contents of a column very fast. The disadvan- */
/* tage is that you need a character buffer that */
/* contains the formatted contents of all        */
/* entries in all columns.                       */
/*-----------------------------------------------*/
{
   int     i,j, k;
   fint    r1, r2;
   fint    maxsize, size;
   fint    nitems, dfault;
   fint    ncols;
   char    mbuf[NORMLEN], mbuf2[NORMLEN];
   char    *tabline = NULL;
   int     tabwidth;
   int     realwidth;
   fchar   Formatstr;
   fint    whites;
   int     flen;
   int     typeok = NO;
   char    *headerline1 = NULL;
   char    *headerline2 = NULL;
   char    *headerline3 = NULL;
   fchar   Destination;
   int     quit;
   fint    removefile = 1;        /* Remove file after it has been printed */
   bool    tohms, todms;
   int     prechms, precdms;      /* Precision in seconds in hms/dms */
   fint    colarray[MAXDISPLAY];
   int     notfiltered;
   int     maxcolumns;
   char    tablename[20];
   bool    tableonly;
   int     maxcols;
   int     nc;


   /* Printer related */

   char    printfilename[120];    /* Name of the file to be printed */
   fint    prnnum;
   fint    cols, rows;



   if (currentcols == 0) {
      anyoutC( 1, "Cannot display, no columns available" );
      return;
   }
   fmake( Formatstr, NORMLEN );
   maxsize  = 0;
   tabwidth = 0;
   whites   = 0;
   i = 0;
   do {
      (void) sprintf( message, "TABCOL%d=", i+1 );
      Key    = tofchar( message );
      if (i == 0) {
         Mes = tofchar("Table, column:        [return to menu]");
      } else {
         Mes = tofchar("Table, column:        [start output]");
      }
      maxcols = 1;
      nc = gettabcols( Key, Mes, REQUEST, maxcols, currentcols, &k, tablename, &tableonly );
      cancel_c( Key );
      if ((nc == 0) && (i == 0)) return;
      if (nc > 0) {
         colarray[i] = k; i++;
         size = (fint) column[k].size;
         if (size > maxsize) maxsize = size;
         displaytableinfo( k );
         tabwidth += getformat( i, k );
         getheader( i, k );
         if (i == 2) {
            nitems = 1;
            dfault = REQUEST;
            whites = 1;
            Key    = tofchar("SPACES=");
            Mes    = tofchar("Give number of spaces between columns:    [1]");
            r2     = userint_c( &whites, &nitems, &dfault, Key, Mes );
            cancel_c( Key );
         }
      }
   } while (nc != 0);
   ncols = i;



   /*-------------------------------------------------------------*/
   /* At this point, the number of lines in the output is         */
   /* known (maxsize). The width of the output is determined      */
   /* by the given formats. Create and prepare a character buffer */
   /*-------------------------------------------------------------*/

   realwidth = tabwidth + (ncols-1)*whites;
   tabwidth += (ncols) * (whites+1);
   tabline = (char *) calloc( (int) (maxsize*tabwidth), sizeof(char) );
   if (tabline == NULL) {
      anyoutC( 1, "Cannot allocate memory for output" );
      return;
   }
   for (i = 0; i < maxsize; i++) {
      for (j = 0; j < tabwidth; j++) {
         tabline[i*tabwidth+j] = ' ';
      }
      tabline[i*tabwidth] = '\0';
   }

   maxcolumns = ncols;
   if (filter) maxcolumns = MYMIN( filtersize, ncols );
   for (i = 0, notfiltered = 0; i < ncols; i++) {
      if (!filter || (filter && filterarray[i])) {
         notfiltered++;
         k    = colarray[i];
         size = (fint) column[k].size;
         if (strstr(column[k].type, "REAL") != NULL) {
            float   *realarray = NULL;

            realarray = column2real(k);
            if (realarray != NULL) {
               typeok = YES;
               tohms = hmsdms( column[k].format, "HMS", &prechms );
               todms = hmsdms( column[k].format, "DMS", &precdms );
               for (j = 0; j < (int) size; j++) {
                  double   ddummy;
                  if (realarray[j] == Fblank) {
                     ddummy = Dblank;
                  } else {
                     ddummy = (double) realarray[j];
                  }
                  if (tohms) {
                     flen = convhmsdms(column[k].format, &ddummy, Formatstr, "HMS", (fint)prechms);
                  } else {
                     if (todms) {
                        flen = convhmsdms(column[k].format, &ddummy, Formatstr, "DMS", (fint)precdms);
                     } else {
                        flen = printusing_c( tofchar(column[k].format),
                                             &ddummy,
                                             Formatstr );
                     }
                  }
                  FtoCstr( Formatstr, mbuf, NORMLEN );
                  addtostr( &tabline[j*tabwidth], mbuf, flen, whites );
               }
            } else return;
            free( realarray );
         }
         if (strstr(column[k].type, "INT") != NULL) {
            fint   *fintarray = NULL;
            fintarray = column2fint( k );
            if (fintarray != NULL) {
               typeok = YES;
               for (j = 0; j < (int) size; j++) {
                  double   ddummy;
                  ddummy = (double) fintarray[j];
                  flen = printusing_c( tofchar(column[k].format),
                                       &ddummy,
                                       Formatstr );
                  FtoCstr( Formatstr, mbuf, NORMLEN );
                  addtostr( &tabline[j*tabwidth], mbuf, flen, whites );
               }
            } else return;
            free( fintarray );
         }

         if (strstr(column[k].type, "DBLE") != NULL) {
            double   *doublearray = NULL;
            doublearray = column2double(k);
            if (doublearray != NULL) {
               typeok = YES;
               tohms = hmsdms( column[k].format, "HMS", &prechms );
               todms = hmsdms( column[k].format, "DMS", &precdms );
               for (j = 0; j < (int) size; j++) {
                  double   ddummy;
                  ddummy = doublearray[j];
                  if (tohms) {
                     flen = convhmsdms(column[k].format, &ddummy, Formatstr, "HMS", (fint)prechms);
                  } else {
                     if (todms) {
                        flen = convhmsdms(column[k].format, &ddummy, Formatstr, "DMS", (fint)precdms);
                     } else {
                        flen   =  printusing_c( tofchar(column[k].format),
                                                &ddummy,
                                                Formatstr );
                     }
                  }
                  FtoCstr( Formatstr, mbuf, NORMLEN );
                  addtostr( &tabline[j*tabwidth], mbuf, flen, whites );
               }
            } else return;
            free( doublearray );
         }

         if (strncmp( "CHAR", column[k].type, 4) == 0) {
            fchar    Fchararray;
            char     *chararray = NULL;
            int      nchar;
            int      f;
            chararray = column2char( k, &nchar );
            if (chararray != NULL) {
               typeok = YES;
               Fchararray.a = chararray;
               Fchararray.l = nchar;
               for (j = 0; j < (int) size; j++) {
                  char  firstchar = column[k].format[0];
                  flen = strlen( column[k].format );
                  /* Do not allow spaces at end of string */
                  f = 1;
                  while ( !isgraph(Fchararray.a[(j+1)*nchar-f]) && (f <= nchar) ) {
                     Fchararray.a[(j+1)*nchar-f] = '\0';
                     f++;
                  }
                  if (firstchar == '-') {
                     (void) sprintf( mbuf, "%-*.*s",
                                     flen, MYMIN( nchar, flen),
                                     &Fchararray.a[j*nchar] );
                  } else {
                     (void) sprintf( mbuf, "%*.*s",
                                     flen, MYMIN( nchar, flen),
                                     &Fchararray.a[j*nchar] );
                  }
                  addtostr( &tabline[j*tabwidth], mbuf, flen, whites );
               }
            } else return;
            free( chararray );
         }

         if (strstr(column[k].type, "LOG") != NULL) {
            bool   *boolarray = NULL;
            boolarray = column2log(k);
            if (boolarray != NULL) {
               typeok = YES;
               for (j = 0; j < (int) size; j++) {
                  char  firstchar = column[k].format[0];
                  char  truefalse[6];

                  flen = strlen( column[k].format );
                  boolarray[j] = tobool(boolarray[j]);
                  if (boolarray[j]) {
                     (void) strcpy( truefalse, "TRUE" );
                  } else {
                     (void) strcpy( truefalse, "FALSE" );
                  }
                  if (firstchar == '-') {
                     (void) sprintf( mbuf, "%-*.*s",
                                     flen, MYMIN( flen, 5 ),
                                     truefalse );
                  } else {
                     (void) sprintf( mbuf, "%*.*s",
                                     flen, MYMIN( flen, 5 ),
                                     truefalse );
                  }
                  addtostr( &tabline[j*tabwidth], mbuf, flen, whites );
               }
            } else return;
            free( boolarray );
         }
         if (typeok) {
            /* Create space for empty entries, because not all tables need to be */
            /* equally long. */
            for (j = size; j < (int) maxsize; j++) {
               (void) sprintf( mbuf2, "%*.0s", strlen(column[k].format)+whites, " " );
               (void) strcat( &tabline[j*tabwidth], mbuf2 );
            }
         } else {
            anyoutC( 1, "Data type not known!" );
            return;
         }
      } /* If no filter or filter and not filtered entry */
   } /* End of all columns? */
   if (filter) displayfilter( filtercol, maxcolumns - notfiltered );

   /* Create a header for the table */

   headerline1 = (char *) calloc( (int) tabwidth+1, sizeof(char) );
   headerline2 = (char *) calloc( (int) tabwidth+1, sizeof(char) );
   headerline3 = (char *) calloc( (int) tabwidth+1, sizeof(char) );
   if ( (headerline1 == NULL) || (headerline2 == NULL) || (headerline2 == NULL) ) {
      anyoutC( 1, "Cannot allocate memory to display table header" );
   } else {
      for (i = 0; i < ncols; i++) {
         k = colarray[i];
         if (hmsdms( column[k].format, "HMS", &prechms )) {
            flen = HMSLEN + prechms;
         } else {
            if (hmsdms( column[k].format, "DMS", &precdms )) {
               flen = DMSLEN + precdms;
            } else {
               flen = strlen(column[k].format);
            }
         }
         (void) sprintf( headerline1,
                 "%.*s%*.*s%*.0s",
                  strlen(headerline1), headerline1,
                  flen, flen,
                  column[k].header,
                  whites, " " );
         (void) sprintf( headerline2,
                 "%.*s%*.*s%*.0s",
                  strlen(headerline2), headerline2,
                  flen, flen,
                  column[k].units,
                  whites, " " );
      }
   }
   memset( headerline3, '=', realwidth );

   prnnum = 0;
   fmake( Destination, 20 );
   quit = NO;
   do {
      Key = tofchar( "DESTINATION=" );
      Mes = tofchar( "[S]creen,  (F)ile,  (P)rinter,  (Q)uit" );
      clearstr( Destination );
      (void) strcpy( Destination.a, "S" );
      nitems = 1;
      dfault = REQUEST;
      r1 = usercharu_c( Destination, &nitems, &dfault, Key, Mes );
      cancel_c( Key );
      for (j = 0; j < nelc_c( Destination ); j++) {
         if (Destination.a[j] == 'Q') {
            quit = YES;
            break;
         }
         if (Destination.a[j] == 'S') {
            anyoutC( 3, " " );
            anyoutC( 3, headerline1 );
            anyoutC( 3, headerline2 );
            anyoutC( 3, headerline3 );
            for (i = 0; i < maxsize; i++) {
               anyoutC( 3, &tabline[i*tabwidth] );
            }
         }
         if (Destination.a[j] == 'F') {
            exportfile = openfile( message );
            if (exportfile != NULL) {
               fprintf( exportfile, "%s\n", headerline1 );
               fprintf( exportfile, "%s\n", headerline2 );
               fprintf( exportfile, "%s\n", headerline3 );
               for (i = 0; i < maxsize; i++) {
                  fprintf( exportfile, "%s\n", &tabline[i*tabwidth] );
               }
               fclose(exportfile);
            }
         }
         if (Destination.a[j] == 'P') {
            /* Ask for printer */
            Key     = tofchar("PRINTER=");
            Mes     = tofchar("Give number of printer ");
            dfault  = HIDDEN;
            nitems  = 1;
            r1      = userint_c( &prnnum, &nitems, &dfault, Key, Mes );
            showdev = 3;
            /*-------------------------------------------------------------------*/
            /* Get number of an available printer. Outside this loop the default */
            /* printer (prnnum) is set to 0. Therefore, the first time always a  */
            /* printer menu is presented unless the hidden keyword PRINTER= is   */
            /* specified with a valid printer number.                            */
            /*-------------------------------------------------------------------*/
            prnnum = prnmenu( Key, &showdev, &prnnum, &cols, &rows );
            if (cols > 0) {
               tmpnam( printfilename );         /* Create unique name for the printer file */
               exportfile = fopen( printfilename, "w");
               if (exportfile != NULL) {
                  fprintf( exportfile, "%s\n", headerline1 );
                  fprintf( exportfile, "%s\n", headerline2 );
                  fprintf( exportfile, "%s\n", headerline3 );
                  for (i = 0; i < maxsize; i++) {
                     fprintf( exportfile, "%s\n", &tabline[i*tabwidth] );
                  }
                  fclose(exportfile);
               } else {
                  anyoutC( 1, "Cannot open file!" );
               }
               removefile = YES;
               r1 = prntract_c( &prnnum, tofchar(printfilename), &removefile );
            } else {
               anyoutC( 1, "No columns available!" );
            }
         }
      }
   } while (!quit);
   if (headerline1 != NULL) free( headerline1 );
   if (headerline2 != NULL) free( headerline2 );
   if (headerline3 != NULL) free( headerline3 );
}



static void plotcols( fint currentcols )
/*---------------------------------------------*/
/* Ask columns and plot contents.              */
/*---------------------------------------------*/
{
   int      j, kx, ky;
   fint     r1;
   fint     sizeX = 0;
   fint     sizeY, size;
   fint     errsizeX, errsizeY;
   floatptr Yarray = NULL;
   floatptr Xarray = NULL;
   floatptr Xerror = NULL;
   floatptr Yerror = NULL;
   floatptr Xerrup = NULL;
   floatptr Xerrlo = NULL;
   floatptr Yerrup = NULL;
   floatptr Yerrlo = NULL;
   float    Xmin=0.0, Xmax=0.0;
   float    Ymin=0.0, Ymax=0.0;
   fint     nitems;
   fint     dfault;
   char     xunits[VARLEN];
   float    delta;
   bool     connect;
   bool     Xbars, Ybars;
   bool     calcX;
   int      xerr, yerr;
   fchar    Comment;
   char     tablename[20];
   bool     tableonly;
   int      maxcols;
   int      nc;



   fmake( Comment, NORMLEN );
   if (currentcols == 0)
   {
      anyoutC( 1, "Cannot plot, no columns available" );
      return;
   }
   do
   {
      Key     = tofchar("YTABCOL=");
      Mes     = tofchar("Table, col. for Y values:        [return to menu]");
      maxcols = 1;
      nc = gettabcols( Key, Mes, REQUEST, maxcols, currentcols, &ky, tablename, &tableonly );
      cancel_c( Key );
      if (nc == 0)
         return;
      else
      {
         displaytableinfo( ky );
         sizeY = (fint) fillrealarray( ky, &Yarray );
         if (Yarray != NULL)
         {
            Key     = tofchar("XTABCOL=");
            Mes     = tofchar("Table, col. for X values:  [Create X column]");
            maxcols = 1;
            nc      = gettabcols( Key, Mes, REQUEST, maxcols, currentcols, &kx, tablename, &tableonly );
            cancel_c( Key );
            if (nc == 0)
            /*--------------------------------------------------*/
            /* No x column was given.                           */
            /*--------------------------------------------------*/
            {
               int       left = 0;
               calcX   = YES;
               connect = YES;
               sizeX   = sizeY;
               Xarray  = (float *) calloc( (int) sizeX, sizeof(float) );
               if (Xarray == NULL)
               {
                  anyoutC( 1, "Cannot allocate memory to plot column" );
                  return;
               }
               Xmax   = (float) sizeY;
               nitems = sizeY;
               dfault = REQUEST;
               Xmin   = 1.0;
               Xmax   = (float) sizeY;
               Key    = tofchar("XVALUES=");
               (void) sprintf( message, "Give %d values for X axis:      [calculated]", nitems );
               Mes    = tofchar( message );
               r1     = userreal_c( Xarray, &nitems, &dfault, Key, Mes );
               left   = nitems - r1;
               cancel_c( Key );
               if (r1 == 0)
               {
                  Xmin   = 1.0; Xmax = (float) nitems;
                  delta  = (Xmax - Xmin) / ((float)sizeY - 1);
                  for (j = 0; j < (int) sizeY; j++)
                     Xarray[j] = Xmin + j*delta;
               }
               else
               {
                  while(left != 0)
                  {
                     (void) sprintf( message, "Still need %d values:      [calculated]", left );
                     Mes   = tofchar( message );
                     r1    = userreal_c( &Xarray[nitems-left], &nitems, &dfault, Key, Mes );
                     left -= r1;
                     cancel_c( Key );
                  }
               }
               (void) strcpy( xunits, "X" );
            }
            else
            /*--------------------------------------------------*/
            /* X column was given given by user.                */
            /*--------------------------------------------------*/
            {
               calcX   = NO;
               connect = NO;
               /* Get the x-values */
               (void) strcpy( xunits, column[kx].units );
               displaytableinfo( kx );
               sizeX = (fint) fillrealarray( kx, &Xarray );
            }
            if (Xarray != NULL)
            {
               nitems  = 1;
               connect = toflog( connect );
               Key     = tofchar("CONNECT=");
               if (connect)
                  Mes     = tofchar("Connect plotted points?    N/[Y]");
               else
                  Mes     = tofchar("Connect plotted points?    [N]/Y");
               dfault  = REQUEST;
               r1      = userlog_c( &connect, &nitems, &dfault, Key, Mes );
               connect = tobool( connect );
               cancel_c( Key );

               /* Read in columns representing errors in the data */

               Key     = tofchar("EYTABCOL=");
               Mes     = tofchar("Table, col. for error in Y values:    [No Y error bar]");
               maxcols = 1;
               nc      = gettabcols( Key, Mes, REQUEST, maxcols, currentcols, &yerr, tablename, &tableonly );
               cancel_c( Key );
               if (nc == 0)
                  Ybars = NO;
               else
               {
                  Ybars = YES;
                  errsizeY = (fint) fillrealarray( yerr, &Yerror );
                  if (Yerror == NULL)
                  {
                     anyoutC( 1, "Cannot fill column with Y errors" );
                  }
                  else
                  {
                     if (errsizeY > sizeY) errsizeY = sizeY;
                     if (errsizeY < sizeY) {
                        anyoutC( 1, "Not enough entries in error column" );
                     }
                     Yerrup = (float *) calloc( (int) sizeY, sizeof(float) );
                     Yerrlo = (float *) calloc( (int) sizeY, sizeof(float) );
                     if ( (Yerrup == NULL) || (Yerrlo == NULL) ) {
                        Ybars = NO;
                        free( Yerror );
                        anyoutC(1, "Cannot allocate space for error data!");
                     }
                  }
               }

               if (calcX) dfault = HIDDEN; else dfault = REQUEST;
               Key     = tofchar("EXTABCOL=");
               Mes     = tofchar("Table, col. for error in X values:    [No X error bar]");
               maxcols = 1;
               nc      = gettabcols( Key, Mes, dfault, maxcols, currentcols, &xerr,
                                     tablename,  &tableonly );
               cancel_c( Key );
               if (nc == 0)
                  Xbars = NO;
               else
               {
                  Xbars = YES;
                  errsizeX = (fint) fillrealarray( xerr, &Xerror );
                  if (Xerror == NULL)
                  {
                     anyoutC( 1, "Cannot fill column with X errors" );
                  }
                  else
                  {
                     if (errsizeX > sizeX) errsizeX = sizeX;
                     if (errsizeX < sizeX) {
                        anyoutC( 1, "Not enough entries in error column" );
                     }
                     Xerrup = (float *) calloc( (int) sizeX, sizeof(float) );
                     Xerrlo = (float *) calloc( (int) sizeX, sizeof(float) );
                     if ( (Xerrup == NULL) || (Xerrlo == NULL) ) {
                        Xbars = NO;
                        free( Xerror );
                        anyoutC( 1, "Cannot allocate space for error data!");
                     }
                  }
               }


               /* Plot contents of array(s) and take care of blanks */
               size = MYMIN( sizeX, sizeY );

               if (filter)
               {
                  int v;
                  v = filterreal( Xarray, (int) size );
                  v = filterreal( Yarray, (int) size );
                  if (Xbars) filterreal( Xerror, (int) size );
                  if (Ybars) filterreal( Yerror, (int) size );
                  displayfilter( filtercol, size-v );
                  size = (fint) v;
               }
               minmax1_c( Yarray, &size, &Ymin, &Ymax );
               minmax1_c( Xarray, &size, &Xmin, &Xmax );
               {
                  int    f;
                  float  lomin, upmax;
                  float  dummy;

                  for (f = 0; f < (int) size; f++) {
                     if (Xbars) {
                        Xerrup[f] = Xarray[f] + Xerror[f];
                        Xerrlo[f] = Xarray[f] - Xerror[f];
                     }
                     if (Ybars) {
                        Yerrup[f] = Yarray[f] + Yerror[f];
                        Yerrlo[f] = Yarray[f] - Yerror[f];
                     }
                  }
                  if (Xbars) {
                     minmax1_c( Xerrup, &size, &dummy, &upmax );
                     minmax1_c( Xerrlo, &size, &lomin, &dummy );
                     if (upmax != Fblank) Xmax = upmax;
                     if (lomin != Fblank) Xmin = lomin;
                  }
                  if (Ybars) {
                     minmax1_c( Yerrup, &size, &dummy, &upmax );
                     minmax1_c( Yerrlo, &size, &lomin, &dummy );
                     if (upmax != Fblank) Ymax = upmax;
                     if (lomin != Fblank) Ymin = lomin;
                  }
               }

               if ( (Xmin == Fblank) || (Xmax == Fblank) ||
                  (Ymin == Fblank) || (Ymax == Fblank) ) {
                  anyoutC( 1, "Cannot draw frame (min or max is blank)" );
               } else {
                  int    f;
                  fint   g = 0;
                  float  one = 1.0;
                  fchar  Ttitle;
                  bool   newframe;

                  newframe = toflog(YES);
                  nitems   = 1;
                  dfault   = HIDDEN;
                  Key      = tofchar("NEWFRAME=");
                  Mes      = tofchar("Advance to new (plot) page?     [Y]/N");
                  r1       = userlog_c( &newframe, &nitems, &dfault, Key, Mes );
                  newframe = tobool( newframe );
                  if (newframe) {
                     dfault   = REQUEST;
                     Key      = tofchar("HEADER=");
                     Mes      = tofchar("Give text as header above plot:   [None]" );
                     Ttitle.a = message;
                     Ttitle.l = 80;
                     r1       = usertext_c( Ttitle, &dfault, Key, Mes );
                     cancel_c( Key );
                     message[r1] = '\0';
                     drawframe( &Xmin, &Ymin, &Xmax, &Ymax,
                                xunits, column[ky].units, message );
                  }


                  for (f = 0; f < (int) size; f++) {
                    if ( (Xarray[f] != Fblank) && (Yarray[f] != Fblank) ) {
                       Xarray[g] = Xarray[f]; Yarray[g] = Yarray[f];
                       if (Xbars) {
                          Xerrup[g] = Xarray[f] + Xerror[f];
                          Xerrlo[g] = Xarray[f] - Xerror[f];
                        }
                       if (Ybars) {
                          Yerrup[g] = Yarray[f] + Yerror[f];
                          Yerrlo[g] = Yarray[f] - Yerror[f];
                       }
                       g++;
                     } else {
                        pgsci_c( &foreground );
                        pgpt_c( &g, Xarray, Yarray, &symbol );
                        if (connect) {
                           pgsci_c( &red );
                           pgline_c( &g, Xarray, Yarray );
                        }
                        if (Xbars || Ybars) pgsci_c( &blue );
                        if (Xbars) pgerrx_c( &g, Xerrup, Xerrlo, Yarray, &one );
                        if (Ybars) pgerry_c( &g, Xarray, Yerrup, Yerrlo, &one );
                        g = 0;
                     }
                  }
                  if (g != 0) {   /* Some or all left? */
                     pgsci_c( &foreground );
                     pgpt_c( &g, Xarray, Yarray, &symbol );
                     if (connect) {
                        pgsci_c( &red );
                        pgline_c( &g, Xarray, Yarray );
                     }
                     if (Xbars || Ybars) pgsci_c( &blue );
                     if (Xbars) pgerrx_c( &g, Xerrup, Xerrlo, Yarray, &one );
                     if (Ybars) pgerry_c( &g, Xarray, Yerrup, Yerrlo, &one );
                  }
                  if (Xbars) {
                     free(Xerrup);
                     free(Xerrlo);
                  }
                  if (Ybars) {
                     free(Yerrup);
                     free(Yerrlo);
                  }
               }
               free( Xarray );
            }
            free( Yarray );
         }
         do {
            fint       r2, r3;
            float      XYpos[2];
            float      angle, fjust;

            Comment.l= NORMLEN;
            clearstr( Comment );
            dfault   = REQUEST;
            Key      = tofchar("COMMENT=");
            Mes      = tofchar("Give text as comment in plot:    [No comment]");
            r1       = usertext_c( Comment, &dfault, Key, Mes );
            Comment.l= r1;
            cancel_c( Key );
            if (r1 > 0) {
               do {
                  XYpos[0] = Xmin + 0.08 * (Xmax - Xmin);             /* Upper left corner */
                  XYpos[1] = Ymin + 0.92 * (Ymax - Ymin);
                  sprintf( message, "Give X, Y in plot coordinates:    [%f %f]",
                           XYpos[0], XYpos[1] );
                  Key      = tofchar("COMPOS=");
                  Mes      = tofchar( message );
                  nitems   = 2;
                  dfault   = REQUEST;
                  r2       = userreal_c( XYpos, &nitems, &dfault, Key, Mes );
                  agreed   = ((r2 == 0) || (r2 == 2));
                  if (!agreed) {
                     reject_c( Key, tofchar("Only X entered") );
                  } else {
                     cancel_c( Key );
                     nitems = 1;
                     dfault = HIDDEN;
                     angle  = 0.0;
                     Key    = tofchar("ANGLE=");
                     Mes    = tofchar("Angle in degrees:     [0]" );
                     r3     = userreal_c( &angle, &nitems, &dfault, Key, Mes );
                     cancel_c( Key );
                     fjust  = 0.0; /* Left justified */
                     Key    = tofchar("JUST=");
                     Mes    = tofchar("Horz. justification 0.0 .. 1.0:    [0.0]");
                     r3     = userreal_c( &fjust, &nitems, &dfault, Key, Mes );
                     if (fjust < 0.0 ) fjust = 0.0;
                     if (fjust > 1.0 ) fjust = 1.0;
                     pgsci_c( &red );
                     pgptxt_c( &XYpos[0], &XYpos[1], &angle, &fjust, Comment );
                  }
               } while (!agreed);
            }
         } while (r1 != 0);
      }
      pgsci_c( &foreground );
   } while (YES);
   cancel_c(tofchar("NEWFRAME="));
}



static fint preparenewcolumn( fchar Setin, fint *subin, fchar Tablename,
                              fchar Columnname, fchar Type, fchar Units,
                              fchar Comment, char *allowedtypes, int cols,
                              int copycol )
/*---------------------------------------------------------------------------*/
/* Ask destination of new column. This is a complete specification with set, */
/* subset(s), table- and column name, type, units and comment. If a column   */
/* exists then there are defaults for type, units and comment, all from the  */
/* column structure.                                                         */
/*---------------------------------------------------------------------------*/
{
   fchar  Key, Mes;
   fint   r1;
   int    i;
   bool   realtype , dbletype , logtype , inttype , chartype;
   bool   agreed;
   bool   exist = NO;
   int    defcol = -1;
   fint   dfault, nitems;
   fint   nsubs;


   fmake( Key, 20 );
   fmake( Mes, NORMLEN );
   dfault = HIDDEN;               /* Default new set and level is old set and level */
   r1     = usertext_c( Setin, &dfault, tofchar("INSET="), tofchar(" ") );
   (void) sprintf( message, "Give set (, subsets) to put column:  [%.*s]",
                   nelc_c( Setin ), Setin.a );
   dfault = REQUEST;
   subdim = 0;
   Key    = tofchar("OUTSET=");
   Mes    = tofchar(message);
   showdev= 3;
   nsubs  = gdsinp_c( Setin,      /* Name of input set. */
                      subin,      /* Array containing subsets coordinate words. */
                      &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                      &dfault,    /* Default code as is USERxxx. */
                      Key,        /* Keyword prompt. */
                      Mes,        /* Keyword message for the user. */
                      &showdev,   /* Device number (as in ANYOUT). */
                      axnum,      /* Array of size 'maxaxes' containing the axes numbers. */
                                  /* The first elements (upto the dimension of the subset) */
                                  /* contain the axes numbers of the subset, */
                                  /* the other ones contain the axes numbers */
                                  /* outside the subset ordered according to the */
                                  /* specification by the user. */
                      axcount,    /* Number of grids on axes in 'axnum' */
                      &maxaxes,   /* Max. number of axes. */
                                  /* the operation for each subset. */
                      &class,     /* Class 1 is for applications which repeat operations */
                      &subdim );  /* Dimensionality of the subsets for class 1 */
   cancel_c( Key );

   dfault = NONE;
   Key    = tofchar("TABNAME=");
   Mes    = tofchar("Give name of table: ");
   r1     = usertext_c( Tablename, &dfault, Key, Mes );

   dfault = NONE;
   Key    = tofchar("COLNAME=");
   Mes    = tofchar("Give name of column: ");
   r1     = usertext_c( Columnname, &dfault, Key, Mes );


   for (i = 0; i < (int) nsubs; i++) {
      defcol = inlist( cols, Setin, Tablename, Columnname, subin[i] );
      exist  = (defcol >= 0);
      if (exist) break;
   }

   (void) strcpy( message, "Type:" );
   if (strchr( allowedtypes, 'I') != NULL ) (void) strcat( message, " INT"   );
   if (strchr( allowedtypes, 'R') != NULL ) (void) strcat( message, " REAL"  );
   if (strchr( allowedtypes, 'D') != NULL ) (void) strcat( message, " DBLE"  );
   if (strchr( allowedtypes, 'L') != NULL ) (void) strcat( message, " LOG"   );
   if (strchr( allowedtypes, 'C') != NULL ) (void) strcat( message, " CHARn" );
   dfault = REQUEST;
   if (copycol >= 0) {
      (void) str2char( column[copycol].type, Type );
      (void) sprintf( message, "%.*s    [%s]", strlen(message), message,
                      column[copycol].type );
   } else {
      if (exist) {
         (void) sprintf( message, "%.*s    [%s]", strlen(message), message,
                         column[defcol].type );
         (void) str2char( column[defcol].type, Type );
      } else {
         (void) strcat(   message, "       [REAL]" );
         (void) str2char( "REAL", Type );
      }
   }
   Key    = tofchar("TYPE=");
   Mes    = tofchar( message );
   do {
      nitems = 1;
      r1     = usercharu_c( Type, &nitems, &dfault, Key, Mes );
      Type.a[nelc_c(Type)] = '\0';
      realtype = (Type.a[0] == 'R');
      dbletype = (Type.a[0] == 'D');
      inttype  = (Type.a[0] == 'I');
      logtype  = (Type.a[0] == 'L');
      chartype = (Type.a[0] == 'C');
      agreed   = ( strchr( allowedtypes, Type.a[0] ) != NULL );
      if (!agreed) {
         dfault = REQUEST;
         reject_c( Key, tofchar("Wrong type!") );
      } else {
         if (chartype && (r1 != 0)) {
            fint nchar  = 10;
            nitems = 1;
            Key = tofchar("NCHAR=");
            Mes = tofchar("Give max. number of characters:  [10]" );
            r1  = userint_c( &nchar, &nitems, &dfault, Key, Mes );
            cancel_c( Key );
            if (nchar < 1) nchar = 1;
            if (nchar > NCHAR) nchar = NCHAR;
            (void) sprintf( Type.a, "CHAR%d\0", nchar );
         }
         if (realtype) (void) str2char( "REAL", Type );
         if (inttype ) (void) str2char( "INT" , Type );
         if (dbletype) (void) str2char( "DBLE", Type );
         if (logtype ) (void) str2char( "LOG" , Type );
      }
   } while (!agreed);


   if (exist) {
      (void) sprintf( message, "Give units of column data:   [%s]",
                      column[defcol].units );
      (void) str2char( column[defcol].units, Units );
   } else {
      (void) sprintf( message, "Give units of column data:    [none]");
      (void) str2char( " ", Units );
   }
   dfault = REQUEST;
   Key    = tofchar("UNITS=");
   Mes    = tofchar( message );
   r1     = usertext_c( Units, &dfault, Key, Mes );


   if (exist) {
      (void) sprintf( message, "Give comment:   [%s]",
                      column[defcol].comment );
      (void) str2char( column[defcol].comment, Comment );
   } else {
      (void) sprintf( message, "Give comment:           [none]" );
      (void) str2char( " ", Comment );
   }
   dfault = REQUEST;
   Key    = tofchar("COMMENT=");
   Mes    = tofchar( message );
   r1     = usertext_c( Comment, &dfault, Key, Mes );

   cancel_c( tofchar("OUTSET=") );
   cancel_c( tofchar("TABNAME=") );
   cancel_c( tofchar("COLNAME=") );
   cancel_c( tofchar("TYPE=") );
   cancel_c( tofchar("UNITS=") );
   cancel_c( tofchar("COMMENT=") );

   return( nsubs );
}



static bool newcol( fchar Setin, fint subin, fchar Tablename,
                    fchar Columnname, fchar Type, fchar Comment,
                    fchar Units, fint *writestart, int size,
                    int cols, fint *err)
/*----------------------------------------------------------*/
/* Create column (or prepare for appending. One subset only */
/* If a column could not be created, return error < 0       */
/*----------------------------------------------------------*/
{
   int    defcol;
   bool   append;
   bool   exist;
   fint   nitems, dfault;
   fint   r1;
   bool   new;


   new = NO;
   defcol = inlist( cols, Setin, Tablename, Columnname, subin );
   exist  = (defcol >= 0);
   append = toflog( NO );
   if (exist) {
      Key    = tofchar("APPCOL=");
      Mes    = tofchar("Append to existing column:      Y/[N]");
      nitems = 1;
      dfault = REQUEST;
      r1     = userlog_c( &append, &nitems, &dfault, Key, Mes );
      append = tobool( append );
      cancel_c( tofchar("APPCOL=") );
   }

   if (append) {
      (void) str2char(column[defcol].type, Type);   /* MUST BE OF SAME TYPE! */
      *writestart = column[defcol].size + 1;
      column[defcol].size += size;
   } else {
      *writestart = 1;
   }

   if (!exist) {
      new = YES;
      r1  = 0;
      gdsa_crecol_c( Setin,
                     &subin,
                     Tablename,
                     Columnname,
                     Type,
                     Comment,
                     Units,
                     &r1 );
      if (r1 < 0) {
         errmsg( r1 );
         *err = r1;
         return(NO);
      }
      extendlist( cols, Setin, Tablename, Columnname, Comment, Type,
                  Units, subin, size );
   }
   *err = 1;
   return(new);
}




static void importdata( int *currentcols )
/*---------------------------------------------*/
/* Import data from an external Ascii file.    */
/* Ask filename of external Ascii file, ask    */
/* table name, column name, units, data type   */
/* and find out the number of items in the     */
/* external column.                            */
/*---------------------------------------------*/
{
   int    num = 0;
   char   filename[80];
   fint   dfault;
   fint   nitems;
   fchar  Tablename, Columnname;
   fchar  Comment;
   fchar  Type, Units;
   fchar  Key, Mes;
   fint   r1;
   fint   Writestart, Arraylen;
   float  Fdummy;
   int    Idummy;
   double Ddummy;
   fint   nchar = 0;
   fint   colsize;
   bool   import;
   int    cols;
   bool   realtype , dbletype , logtype , inttype , chartype;


   cols = *currentcols;
   fmake( Key,        20 );
   fmake( Mes,        NORMLEN );
   fmake( Tablename,  SHORTLEN );
   fmake( Columnname, SHORTLEN );
   fmake( Units,      VARLEN );
   fmake( Comment,    VARLEN );
   fmake( Type,       VARLEN );
   nsubs = preparenewcolumn( Setin, subin, Tablename,
                             Columnname, Type, Units,
                             Comment, "IRDLC", cols, -1 );
   realtype = (strstr(Type.a, "REAL") != NULL);
   dbletype = (strstr(Type.a, "DBLE") != NULL);
   inttype  = (strstr(Type.a, "INT" ) != NULL);
   logtype  = (strstr(Type.a, "LOG" ) != NULL);
   chartype = (strstr(Type.a, "CHAR") != NULL);
   if (chartype) nchar = atoi( &Type.a[4] );

   importfile = getimportfile( filename );
   import     = (importfile != NULL);
   if (import) {
      /* Find the number of items to read */
      while (!feof(importfile)) {
         message[0] = '\0';
         fgets( message, NORMLEN, importfile);
         if (realtype) r1 = sscanf( message, "%f", &Fdummy );
         if (inttype ) r1 = sscanf( message, "%d", &Idummy  );
         if (dbletype) r1 = sscanf( message, "%d", &Ddummy  );
         if ((logtype) || (chartype) ) {
            if (!feof(importfile)) r1 = 1; else r1 = 0;
         }
         if (r1 == 1) num++;
      }
      fclose( importfile );
   } else {
      /* Ask length */
      nitems  = 1;
      dfault  = REQUEST;
      Key     = tofchar("COLSIZE=");
      Mes     = tofchar("Give number of entries in column:    [return to menu]" );
      r1      = userint_c( &colsize, &nitems, &dfault, Key, Mes );
      cancel_c( Key );
      if (r1 == 0) return;
      num     = (int) colsize;
   }

   if (realtype) floatarray  = (float *)  calloc( num, sizeof(float) );
   if (inttype ) fintarray   = (fint *)   calloc( num, sizeof(fint) );
   if (dbletype) doublearray = (double *) calloc( num, sizeof(double) );
   if (logtype ) boolarray   = (bool *)   calloc( num, sizeof(bool) );
   if (chartype) {
      fchararray.a = (char *) calloc( num*nchar+1, sizeof(char) );
      fchararray.l = nchar;
   }

   if (import) {
      /* Open again to read in data */
      importfile = fopen(filename, "r");
      num = 0;
      while (!feof(importfile)) {
         int len;
         message[0] = '\0';
         fgets( message, NCHAR, importfile);
         if (realtype) r1 = sscanf( message, "%f", &floatarray[num] );
         if (inttype)  {
            r1 = sscanf( message, "%d", &Idummy );
            fintarray[num] = (fint) Idummy;
         }
         if (dbletype) r1 = sscanf( message, "%lf", &doublearray[num] );
         if (chartype) {
            if (!feof(importfile)) {
               int   f;
               r1  = 1;
               len = (int) MYMIN( nchar, strlen(message) );
               /* fgets copies the newline character also, change this character */
               /* to the null character */
               for (f = 0; f < len; f++) {
                  char onechar = message[f];
                  if (onechar == '\n') onechar = '\0';
                  fchararray.a[num*nchar+f] = onechar;
               }
            } else {
               r1 = 0;
            }
         }
         if (logtype) {
            if (!feof(importfile)) {
               r1 = 1;
               if (strpbrk( message, "YJTyjt") != NULL ) {
                  boolarray[num] = toflog( YES );
               } else {
                  boolarray[num] = toflog( NO );
               }
            } else {
               r1 = 0;
            }
         }
         if (r1 == 1) num++;
      } /* End of file encountered */
      (void) sprintf( message, "Converted %d entries from ASCII file", num );
      anyoutC( 3, message );
      fclose(importfile);
   } /* End of import */

   for (i = 0; i < (int) nsubs; i++) {
      bool  new;
      new = newcol( Setin, subin[i], Tablename, Columnname, Type,
                    Comment, Units, &Writestart, num, cols, &r1 );
      if (r1 < 0) {
         sprintf( message, "r1 = %d", r1 ); anyoutC(3, message );
         return;
      } else {
         if (new) {
            cols++;
            Arraylen = num;
         } else {
            Arraylen = (Writestart-1) + num;
         }
      }

      r1 = 0;
      if (realtype) {
         gdsa_wcreal_c( Setin,                /* Name of GDS set. */
                        &subin[i],            /* Subset where table is to be created. */
                        Tablename,            /* Name of GDS table. */
                        Columnname,           /* Name of GDS column. */
                        floatarray,           /* Array containing the data to be written */
                        &Writestart,          /* Row number where to start */
                                              /* writing. If zero data will */
                                              /* be added at the end of the */
                                              /* column. */
                        &Arraylen,            /* Number of rows to write. */
                        &r1 );                /* Error return code. */
         free( floatarray );
      }
      if (inttype) {
         gdsa_wcint_c(  Setin,
                        &subin[i],
                        Tablename,
                        Columnname,
                        fintarray,
                        &Writestart,
                        &Arraylen,
                        &r1 );
         free( fintarray );
      }
      if (dbletype) {
         gdsa_wcdble_c( Setin,
                        &subin[i],
                        Tablename,
                        Columnname,
                        doublearray,
                        &Writestart,
                        &Arraylen,
                        &r1 );
         free( doublearray );
      }
      if (chartype) {
                gdsa_wcchar_c( Setin,
                               &subin[i],
                               Tablename,
                               Columnname,
                               fchararray,
                               &Writestart,
                               &Arraylen,
                               &r1 );
         free( fchararray.a );
      }
      if (logtype) {
                gdsa_wclog_c(  Setin,
                               &subin[i],
                               Tablename,
                               Columnname,
                               boolarray,
                               &Writestart,
                               &Arraylen,
                               &r1 );
         free( boolarray );
      }
      if (r1 < 0) {
         errmsg( r1 );
         anyoutC( 1, "Could not write the data into the column!" );
         return;
      }
   }
   if (*currentcols != cols) sortandprint( cols );
   *currentcols = cols;
}




static void delcoltab( int cols )
/*---------------------------------------------*/
/*---------------------------------------------*/
{
   fint        nitems;
   fint        dfault;
   fint        r1;
   int         i, j;
   int         col;
   fint        err;
   fint        subs;
   int         colarray[MAXDELETE];
   bool        deleted = NO;
   int         len;
   int         tablenum;
   char        tablename[20];
   bool        tableonly;
   int         maxcols;
   int         nc;


   if (cols == 0) {
      anyoutC( 1, "Cannot delete, no columns found" );
      return;
   }
   do {
      maxcols = (fint) cols;
      maxcols = MYMIN( maxcols, MAXDELETE );
      dfault  = REQUEST;
      Key     = tofchar("DTABCOL=");
      Mes     = tofchar("Delete tab (, col(s)):             [return to menu]");
      nc      = gettabcols( Key, Mes, dfault, maxcols, cols, colarray,
                            tablename,  &tableonly );
      cancel_c( Key );


      if (nc == 0) {
         if (deleted) sortandprint( cols );
      } else {
         if (tableonly) {
            bool    ok;
            bool    first;


            first = YES;
            tablenum = colarray[0];
            for (j = 0; j < cols; j++) {
               if ( (strncmp( column[j].tablename, column[tablenum].tablename, SHORTLEN ) == 0 ) &&
                    (strcmp( column[j].setname, column[tablenum].setname) == 0 ) &&
                    (column[j].subset == column[tablenum].subset) ) {
                  len = strlen( column[j].level );
                  sprintf( column[j].level, "%*.*s", len, len, "DELETED" );
               }
            }
            (void) sprintf( message, "Ok to delete ALL columns of %s?     [Y]/N",
                            column[tablenum].tablename );
            ok     = toflog( YES );
            nitems = 1;
            dfault = REQUEST;
            Key    = tofchar("OK=");
            Mes    = tofchar( message );
            r1     = userlog_c( &ok, &nitems, &dfault, Key, Mes );
            ok     = tobool( ok );
            cancel_c( Key );


            subs   = column[tablenum].subset;
            if (ok) {
               err = 0;
               gdsa_deltab_c( tofchar(column[tablenum].setname),
                              &subs,
                              tofchar(column[tablenum].tablename),
                              &err );
               deleted = YES;
               if (err < 0) errmsg ( err );
            }
         } else {
            for (i = 0; i < nc; i++) {
               col = colarray[i];
               err = 0;
               subs = column[col].subset;
               (void) sprintf( message, "TABLE deletes table:%s, column:%s",
                               column[col].tablename,
                               column[col].columnname );
               anyoutC( 1, message );
               gdsa_delcol_c( tofchar(column[col].setname),
                              &subs,
                              tofchar(column[col].tablename),
                              tofchar(column[col].columnname),
                              &err );
               deleted = YES;
               len = strlen( column[col].level );
               sprintf( column[col].level, "%*.*s", len, len, "DELETED" );
               if (err < 0) errmsg ( err );
            }
         }
      }
   } while(nc != 0);
}




static void copycol( int *currentcols )
/*-------------------------------------------------*/
/* Copy data from one column to another, or append */
/*-------------------------------------------------*/
{
   int     cols;
   fint    dfault;
   fint    r1;
   bool    exist;
   int     i,j,k;
   int     nsubs;
   bool    realtype , dbletype , logtype , inttype , chartype;
   bool    nrealtype , ndbletype , nlogtype , ninttype , nchartype;
   char    allowedtype[5];
   fint    writestart;
   fint    arraylen;

   fint    *fintarray   = NULL;
   float   *floatarray  = NULL;
   double  *doublearray = NULL;
   bool    *boolarray   = NULL;
   char    *chararray   = NULL;
   fchar   Fchararray;
   int     nchar, ncharnew;
   int     v = 0;
   int     w;
   fchar   Tablename, Columnname;
   fchar   Units, Type;
   fchar   Comment;
   char    mbuf[20];
   int     size;
   bool    new;
   char    tablename[20];
   bool    tableonly;
   int     maxcols;
   int     nc;



   if (currentcols == 0) {
      anyoutC( 1, "No columns available" );
      return;
   }
   cols = *currentcols;
   fmake( Tablename,  SHORTLEN );
   fmake( Columnname, SHORTLEN );
   fmake( Units,      VARLEN );
   fmake( Comment,    VARLEN );
   fmake( Type,       VARLEN );
   do {
      (void) strcpy( message, "Table, column to copy:     [return to menu]");
      (void) strcpy( mbuf, "CTABCOL=" );
      Mes     = tofchar(message);
      Key     = tofchar(mbuf);
      maxcols = 1;
      dfault  = REQUEST;
      nc      = gettabcols( Key, Mes, dfault, maxcols, *currentcols, &k,
                            tablename,  &tableonly );
      cancel_c( Key );
      if (nc == 0) {   /* Abort loop and update table directory */
         if ((*currentcols) != cols) sortandprint( cols );
         *currentcols = cols;
         return;
      }
      displaytableinfo( k );
      chartype = (strncmp( "CHAR", column[k].type, 4) == 0);
      realtype = (strstr(column[k].type, "REAL") != NULL);
      dbletype = (strstr(column[k].type, "DBLE") != NULL);
      inttype  = (strstr(column[k].type, "INT" ) != NULL);
      logtype  = (strstr(column[k].type, "LOG" ) != NULL);
      exist = ( realtype || dbletype || inttype || chartype || logtype );
      if (!exist) {
         reject_c( Key, tofchar("Wrong type!") );
      } else {
         if (chartype) (void) strcpy( allowedtype, "C" );
         if (inttype)  (void) strcpy( allowedtype, "IRDL" );
         if (realtype) (void) strcpy( allowedtype, "IRDL" );
         if (dbletype) (void) strcpy( allowedtype, "IRDL" );
         if (logtype)  (void) strcpy( allowedtype, "IRDL" );
         /* Setin and subin are global */
         nsubs = preparenewcolumn( Setin, subin, Tablename,
                                   Columnname, Type, Units,
                                   Comment, allowedtype, cols, k );
         /* What is the new type? */
         nrealtype = (Type.a[0] == 'R');
         ndbletype = (Type.a[0] == 'D');
         ninttype  = (Type.a[0] == 'I');
         nlogtype  = (Type.a[0] == 'L');
         nchartype = (Type.a[0] == 'C');
          size = (fint) column[k].size;
         if (inttype) {
            fintarray = column2fint( k );
            if (fintarray == NULL) {
               return;
            }
            if (filter) {
               for(w = 0, v = 0; w < MYMIN(filtersize, size); w++) {
                  if (filterarray[w]) fintarray[v++] = fintarray[w];
               }
            }
         }
         if (realtype) {
            floatarray = column2real( k );
            if (floatarray == NULL) {
               return;
            }
            if (filter) {
               for(w = 0, v = 0; w < MYMIN(filtersize, size); w++) {
                  if (filterarray[w]) floatarray[v++] = floatarray[w];
               }
            }
         }
         if (dbletype) {
            doublearray = column2double( k );
            if (doublearray == NULL) {
               return;
            }
            if (filter) {
               for(w = 0, v = 0; w < MYMIN(filtersize, size); w++) {
                  if (filterarray[w]) doublearray[v++] = doublearray[w];
               }
            }
         }
         if (logtype) {
            boolarray = column2log( k );
            if (boolarray == NULL) {
               return;
            }
            if (filter) {
               for(w = 0, v = 0; w < MYMIN(filtersize, size); w++) {
                  if (filterarray[w]) boolarray[v++] = boolarray[w];
               }
            }
         }
         if (chartype) {
            chararray = column2char( k, &nchar );
            if (chararray == NULL) {
               return;
            } else {
               ncharnew = atoi( &Type.a[4] );
               ncharnew = MYMIN(nchar, ncharnew);
               Fchararray.a = chararray;
               Fchararray.l = nchar;
            }
            if (filter) {
               for(w = 0, v = 0; w < MYMIN(filtersize, size); w++) {
                  if (filterarray[w]) {
                     int  f;
                     for (f = 0; f < ncharnew; f++) {
                        Fchararray.a[(v*ncharnew)+f] = Fchararray.a[(w*nchar)+f];
                     }
                     v++;
                  }
               }
            }
         }
         if (filter) {
            displayfilter( filtercol, size-v );
            size = v;
         }
          for (i = 0; i < (int) nsubs; i++) {   /* Loop over all output subsets */
             new = newcol( Setin, subin[i], Tablename, Columnname, Type,
                           Comment, Units, &writestart, size, cols, &r1 );
             if (r1 < 0) {
                return;
             } else {
                if (new) {
                  cols++;
                  arraylen = size;
               } else arraylen = (writestart-1) + size;
            }
               if (inttype) {
                  if (ninttype) {
                     r1 = 0;
                     gdsa_wcint_c( Setin, &subin[i], Tablename, Columnname,
                                   fintarray, &writestart, &arraylen, &r1 );
                  }
                  if (nrealtype) {  /* Copy to float array */
                     float         *floatarray = NULL;
                     floatarray  = (float *)  calloc( (int) size, sizeof(float) );
                     for (j = 0; j < (int) size; j++) floatarray[j] = (float) fintarray[j];
                     r1 = 0;
                     gdsa_wcreal_c( Setin, &subin[i], Tablename, Columnname,
                                    floatarray, &writestart, &arraylen, &r1 );
                     free( floatarray );
                  }
                  if (ndbletype) {
                     double     *doublearray = NULL;
                     doublearray  = (double *)  calloc( (int) size, sizeof(double) );
                     for (j = 0; j < (int) size; j++) doublearray[j] = (double) fintarray[j];
                     r1 = 0;
                     gdsa_wcdble_c( Setin, &subin[i], Tablename, Columnname,
                                    doublearray, &writestart, &arraylen, &r1 );
                     free( doublearray );
                  }
                  if (nlogtype) {
                     bool       *boolarray = NULL;
                     boolarray  = (bool *)  calloc( (int) size, sizeof(bool) );
                     for (j = 0; j < (int) size; j++) boolarray[j] = toflog(fintarray[j]);
                     r1 = 0;
                     gdsa_wclog_c( Setin, &subin[i], Tablename, Columnname,
                                   boolarray, &writestart, &arraylen, &r1 );
                     free( boolarray );
                  }
                  free( fintarray );
            } /* end of inttype */
             if (realtype) {
                  if (ninttype) {
                     fint       *fintarray = NULL;
                     fintarray  = (fint *)  calloc( (int) size, sizeof(fint) );
                     for (j = 0; j < (int) size; j++) fintarray[j] = (fint) floatarray[j];
                     r1 = 0;
                     gdsa_wcint_c( Setin, &subin[i], Tablename, Columnname,
                                   fintarray, &writestart, &arraylen, &r1 );
                     free( fintarray );
                  }
                  if (nrealtype) {  /* Copy to float array */
                     r1 = 0;
                     gdsa_wcreal_c( Setin, &subin[i], Tablename, Columnname,
                                    floatarray, &writestart, &arraylen, &r1 );
                  }
                  if (ndbletype) {
                     double     *doublearray = NULL;
                     doublearray  = (double *)  calloc( (int) size, sizeof(double) );
                     for (j = 0; j < (int) size; j++) doublearray[j] = (double) floatarray[j];
                     gdsa_wcdble_c( Setin, &subin[i], Tablename, Columnname,
                                    doublearray, &writestart, &arraylen, &r1 );
                     free( doublearray );
                  }
                  if (nlogtype) {
                     bool       *boolarray = NULL;
                     boolarray  = (bool *)  calloc( (int) size, sizeof(bool) );
                     for (j = 0; j < (int) size; j++) boolarray[j] = toflog(floatarray[j]);
                     gdsa_wclog_c( Setin, &subin[i], Tablename, Columnname,
                                   boolarray, &writestart, &arraylen, &r1 );
                     free( boolarray );
                  }
                  free( floatarray );
            }
            if (dbletype) {
                  if (ninttype) {
                     fint       *fintarray = NULL;
                     fintarray  = (fint *)  calloc( (int) size, sizeof(fint) );
                     for (j = 0; j < (int) size; j++) fintarray[j] = (fint) doublearray[j];
                     r1 = 0;
                     gdsa_wcint_c( Setin, &subin[i], Tablename, Columnname,
                                   fintarray, &writestart, &arraylen, &r1 );
                     free( fintarray );
                  }
                  if (nrealtype) {  /* Copy to float array */
                     float         *floatarray = NULL;
                     floatarray  = (float *)  calloc( (int) size, sizeof(float) );
                     for (j = 0; j < (int) size; j++) floatarray[j] = (float) doublearray[j];
                     r1 = 0;
                     gdsa_wcreal_c( Setin, &subin[i], Tablename, Columnname,
                                    floatarray, &writestart, &arraylen, &r1 );
                     free( floatarray );
                   }
                  if (ndbletype) {
                     r1 = 0;
                     gdsa_wcdble_c( Setin, &subin[i], Tablename, Columnname,
                                    doublearray, &writestart, &arraylen, &r1 );
                  }
                  if (nlogtype) {
                     bool       *boolarray = NULL;
                     boolarray  = (bool *)  calloc( (int) size, sizeof(bool) );
                     for (j = 0; j < (int) size; j++) boolarray[j] = toflog(doublearray[j]);
                     gdsa_wclog_c( Setin, &subin[i], Tablename, Columnname,
                                   boolarray, &writestart, &arraylen, &r1 );
                     free( boolarray );
                  }
                  free( doublearray );
            }
            if (logtype) {
                  if (ninttype) {
                     fint       *fintarray = NULL;
                     fintarray  = (fint *)  calloc( (int) size, sizeof(fint) );
                     for (j = 0; j < (int) size; j++) fintarray[j] = (fint) boolarray[j];
                     r1 = 0;
                     gdsa_wcint_c( Setin, &subin[i], Tablename, Columnname,
                                   fintarray, &writestart, &arraylen, &r1 );
                     free( fintarray );
                  }
                  if (nrealtype) {  /* Copy to float array */
                     float         *floatarray = NULL;
                     floatarray  = (float *)  calloc( (int) size, sizeof(float) );
                     for (j = 0; j < (int) size; j++) floatarray[j] = (float) boolarray[j];
                     r1 = 0;
                     gdsa_wcreal_c( Setin, &subin[i], Tablename, Columnname,
                                    floatarray, &writestart, &arraylen, &r1 );
                     free( floatarray );
                   }
                  if (ndbletype) {
                     double     *doublearray = NULL;
                     doublearray  = (double *)  calloc( (int) size, sizeof(double) );
                     for (j = 0; j < (int) size; j++) doublearray[j] = (double) boolarray[j];
                     gdsa_wcdble_c( Setin, &subin[i], Tablename, Columnname,
                                    doublearray, &writestart, &arraylen, &r1 );
                     free( doublearray );
                  }
                  if (nlogtype) {
                     r1 = 0;
                     gdsa_wclog_c( Setin, &subin[i], Tablename, Columnname,
                                   boolarray, &writestart, &arraylen, &r1 );
                  }
                  free( boolarray );
            }
            if (chartype) {
               if (nchartype) {
                  r1 = 0;
                  gdsa_wcchar_c( Setin, &subin[i], Tablename, Columnname,
                                 Fchararray, &writestart, &arraylen, &r1 );
               }
               free( chararray );
            }
         } /* end of all subsets */
      } /* end if correct type */
   } while(1);
}




static void getcoldata( fint *len, fint numpars, int cols )
/*-----------------------------------------------------------------*/
/* Get the table, column number per parameter in the expression.   */
/* Number of columns always >= number of parameters in expression. */
/*-----------------------------------------------------------------*/
{
   int     i, j;
   fint    dfault;
   int     k;
   int     colnumber[MAXPARS];
   int     cn = 0;
   fint    minsize;
   int     exist = NO;
   int     dataoffset;
   char    mbuf[20];
   bool    realtype, inttype, dbletype;
   char    tablename[20];
   bool    tableonly;
   int     maxcols;
   int     nc;


   minsize = 0;
   for (i = 0; i < numpars; i++) {
      do {
         (void) sprintf( message, "Table, column for $%d:", i+1 );
         (void) sprintf( mbuf, "TABCOL%d=", i+1 );
         Mes     = tofchar(message);
         Key     = tofchar(mbuf);
         maxcols = 1;
         dfault  = NONE;
         nc      = gettabcols( Key, Mes, dfault, maxcols, cols, &k,
                               tablename,  &tableonly );
         cancel_c( Key );
         realtype = (strstr(column[k].type, "REAL") != NULL);
         dbletype = (strstr(column[k].type, "DBLE") != NULL);
         inttype  = (strstr(column[k].type, "INT" ) != NULL);
         exist = ( realtype || dbletype || inttype );
         if (!exist) {
            reject_c( Key, tofchar("Wrong type!") );
         } else {
            if (cn == 0) {
               minsize = (fint) column[k].size;
            } else {
               minsize = (fint) MYMIN( minsize, column[k].size );
            }
            (void) sprintf( message, "$%d = [%s, %s]",
                            i+1,
                            column[k].tablename,
                            column[k].columnname );
            anyoutC( 3, message );
            colnumber[cn++] = k;
         }
      } while (!exist);
   }
   /*---------------------------------------------------------*/
   /* The size of the output array in 'fiedo' gets the length */
   /* of the smallest column                                  */
   /*---------------------------------------------------------*/

   if (minsize == 0) {
      anyoutC( 1, "Size of smallest column == 0" );
      return;
   }
   /*----------------------------------------------------*/
   /* Now we have 'numpars' columns, with size 'minsize' */
   /* and it is possible to allocate 'dataIN'. Take      */
   /* spaecial care if a filter if used.                 */
   /*----------------------------------------------------*/

   if (filter) minsize = MYMIN( minsize, filtersize );

   dataIN = (float *)  calloc( (int) numpars*minsize, sizeof(float) );
   if (dataIN == NULL) {
      anyoutC( 1, "Could not allocate memory for column data" );
      return;
   }
   *len = minsize;

   /*------------------------------------------------------*/
   /* There is an array now with structure numbers for the */
   /* columns to get the data from. From each column       */
   /* 'minsize' items are read. Data is always converted   */
   /* to floats.                                           */
   /*------------------------------------------------------*/

   dataoffset = 0;
   for (i = 0; i < numpars; i++) {
      k = colnumber[i];
      if (strstr(column[k].type, "REAL") != NULL) {
         float    *floatarray = NULL;
         floatarray = column2real(k);
         if (floatarray == NULL) return;
         for (j = 0; j < minsize; j++) {
            dataIN[dataoffset+j] = floatarray[j];
         }
         dataoffset += minsize;
         free( floatarray );
      }
      if (strstr(column[k].type, "DBLE") != NULL) {
         double   *doublearray = NULL;
         doublearray = column2double(k);
         if (doublearray == NULL) return;
         for (j = 0; j < minsize; j++) {
            dataIN[dataoffset+j] = (float) doublearray[j];
         }
         dataoffset += minsize;
         free( doublearray );
      }
      if (strstr(column[k].type, "INT") != NULL) {
         fint    *fintarray = NULL;
         fintarray = column2fint(k);
         if (fintarray == NULL) return;
         for (j = 0; j < minsize; j++) {
            dataIN[dataoffset+j] = (float) fintarray[j];
         }
         dataoffset += minsize;
         free( fintarray );
      }
   }
   if (filter) {
      int   indx;
      int   par;
      int   filt;
      dataoffset = 0;
      for (par = 0, indx = 0; par < numpars; par++) {
         for (filt = 0; filt < minsize; filt++) {
            if (filterarray[filt]) {
               dataIN[indx++] = dataIN[par*minsize+filt];
            }
         }
         if (par == 0) {
            *len = indx;     /* New length of filtered column */
            displayfilter( filtercol, minsize-(*len) );
         }
      }
   }
}




static void putcol( float *values, fint collen, int *currentcols )
/*----------------------------------------------------------------*/
/* Called by the calc routine                                     */
/*----------------------------------------------------------------*/
{
   fchar  Key, Mes;
   fchar  Tablename, Columnname;
   fchar  Comment;
   fchar  Type, Units;
   fint   r1;
   fint   Writestart, Arraylen;
   bool   exist;
   fint   dfault, nitems;
   bool   append;
   bool   realtype , dbletype , logtype , inttype;
   int    w;
   int    defcol = -1;
   int    cols;
   bool   refresh = NO;


   cols = *currentcols;
   fmake( Key,        20 );
   fmake( Mes,        NORMLEN );
   fmake( Tablename,  SHORTLEN );
   fmake( Columnname, SHORTLEN );
   fmake( Units,      VARLEN );
   fmake( Comment,    VARLEN );
   fmake( Type,       VARLEN );

   /* All types allowed, except character type */
   nsubs = preparenewcolumn( Setin, subin, Tablename,
                             Columnname, Type, Units,
                             Comment, "IRDL", cols, -1 );

   /* (void) sprintf( message, "%.*s, %.*s %.*s %.*s %.*s %.*s %d",
                   nelc_c(Setin), Setin.a,
                   nelc_c(Tablename), Tablename.a,
                   nelc_c(Columnname), Columnname.a,
                   nelc_c(Type), Type.a,
                   nelc_c(Units), Units.a,
                   nelc_c(Comment), Comment.a,
                   nsubs );
                   anyoutC(3, message ); */

   /* Loop over all specified subsets */

   refresh = NO;
   for (i = 0; i < (int) nsubs; i++) {
      defcol = inlist( cols, Setin, Tablename, Columnname, subin[i] );
      exist  = (defcol >= 0);
      append = toflog( NO );
      if (exist) {
         Key    = tofchar("APPCOL=");
         Mes    = tofchar("Append to existing column:      Y/[N]");
         nitems = 1;
         dfault = REQUEST;
         r1     = userlog_c( &append, &nitems, &dfault, Key, Mes );
         append = tobool( append );
         cancel_c( tofchar("APPCOL=") );
      }

      if (append) {
         Writestart = column[defcol].size + 1;
         column[defcol].size += (int) collen;
      } else {
         Writestart = 1;
      }

      Arraylen = collen;
      if (!exist) {
         r1 = 0;
         gdsa_crecol_c( Setin,
                        &subin[i],
                        Tablename,
                        Columnname,
                        Type,
                        Comment,
                        Units,
                        &r1 );
         if (r1 < 0) {
            errmsg( r1 );
            return;
         }
         refresh = YES;
         extendlist( cols, Setin, Tablename, Columnname, Comment, Type,
                     Units, subin[i], Arraylen );
         cols++;
      }
      r1 = 0;
      if (exist) {
         realtype = (strstr(column[defcol].type, "REAL") != NULL);
         dbletype = (strstr(column[defcol].type, "DBLE") != NULL);
         inttype  = (strstr(column[defcol].type, "INT" ) != NULL);
         logtype  = (strstr(column[defcol].type, "LOG" ) != NULL);
      } else {
         realtype = (Type.a[0] == 'R');
         dbletype = (Type.a[0] == 'D');
         inttype  = (Type.a[0] == 'I');
         logtype  = (Type.a[0] == 'L');
      }
      r1 = 0;
      if (realtype) {
         gdsa_wcreal_c( Setin,                /* Name of GDS set. */
                        &subin[i],            /* Subset where table is to be created. */
                        Tablename,            /* Name of GDS table. */
                        Columnname,           /* Name of GDS column. */
                        values,               /* Array containing the data to be written */
                        &Writestart,          /* Row number where to start */
                                              /* writing. If zero data will */
                                              /* be added at the end of the */
                                              /* column. */
                        &Arraylen,            /* Number of rows to write. */
                        &r1 );                /* Error return code. */
      }
      if (dbletype) {
         double     *dblevals = NULL;
         dblevals = (double *) calloc( (int) Arraylen, sizeof(double) );
         if (dblevals == NULL) {
            anyoutC( 1, "Cannot allocate memory to convert to double" );
            return;
         }
         for (w = 0; w < (int) Arraylen; w++) dblevals[w] = (double) values[w];
         gdsa_wcdble_c( Setin, &subin[i], Tablename, Columnname,
                        dblevals, &Writestart, &Arraylen, &r1 );
         free( dblevals );
      }
      if (inttype) {
         fint     *fintvals = NULL;
         fintvals = (fint *) calloc( (int) Arraylen, sizeof(fint) );
         if (fintvals == NULL) {
            anyoutC( 1, "Cannot allocate memory to convert to integers" );
            return;
         }
         for (w = 0; w < (int) Arraylen; w++) fintvals[w] = (fint) values[w];
         gdsa_wcint_c( Setin, &subin[i], Tablename, Columnname,
                       fintvals, &Writestart, &Arraylen, &r1 );
         free( fintvals );
      }
      if (logtype) {
         bool     *logvals = NULL;
         logvals = (bool *) calloc( (int) Arraylen, sizeof(bool) );
         if (logvals == NULL) {
            anyoutC( 1, "Cannot allocate memory to convert to logicals" );
            return;
         }
         for (w = 0; w < (int) Arraylen; w++) logvals[w] = toflog(values[w]);
         gdsa_wclog_c( Setin, &subin[i], Tablename, Columnname,
                       logvals, &Writestart, &Arraylen, &r1 );
         free( logvals );
      }
      if (r1 < 0) errmsg( r1 );
   }
   if (refresh) sortandprint( cols );
   *currentcols = cols;
}


static void calc( int *currentcols )
/*--------------------------------------------------------------*/
/* Ask expression and ask for columns to replace the variables. */
/* Specify what is needed to create a new column and put the    */
/* results in that column.                                      */
/*--------------------------------------------------------------*/
{
   fint     len;
   fint     dfault;
   fchar    Expression;
   fint     fid;             /* Function id */
   fint     numpars;         /* Number of columns in the expression */
   fint     errpos;          /* Position of error in expression */
   fint     fieresult;
   int      cols;
   fint     nitems;
   fint     r1;
   fint     colsize;


   cols = *currentcols;
   if (cols == 0) {
      anyoutC( 1, "No columns available" );
      return;
   }

   fmake( Expression, NORMLEN );
   dfault = REQUEST;
   do {
      Key = tofchar( "EXPRESSION=" );
      Mes = tofchar( "F($1, $2,...$n) =            [return to menu]" );
      len = usertext_c( Expression, &dfault, Key, Mes );
      cancel_c( Key );
      if (len == 0) return;

      /* Set function id */
      fid = 0;
      numpars = fieini_c( Expression, &fid, &errpos );
      agreed = ((numpars > 0) && (numpars <= cols) && (numpars < MAXPARS));
      if (!agreed) {
         cancel_c( Key );
         if (numpars == 0) {
            /* Ask length */
            nitems  = 1;
            dfault  = REQUEST;
            Key     = tofchar("COLSIZE=");
            Mes     = tofchar("Give number of entries in column:    [return to menu]" );
            r1      = userint_c( &colsize, &nitems, &dfault, Key, Mes );
            cancel_c( Key );
            if (r1 == 0) return;
            dataIN = (float *)  calloc( (int) colsize, sizeof(float) );
            if (dataIN == NULL) {
               anyoutC( 1, "Could not allocate memory for column data" );
               return;
            } else {
               agreed = YES;
            }
         }
         if (numpars > cols) {
            anyoutC( 1, "More parameters than available columns!");
         }
         if (numpars > MAXPARS) {
            (void) sprintf( message, "Max. allowed number of parameters is %d", MAXPARS );
            anyoutC( 1, message );
         }
         if (numpars < 0) {
            (void) sprintf( message, "Error in expression at or near position %d", errpos );
            anyoutC( 1, message );
         }
      }
   } while (!agreed);

   /*---------------------------------------------------------*/
   /* Loop over the parameters to get the column data in the  */
   /* input array 'dataIN'. The output is always a real array */
   /* dataOUT. The length of the smallest column will be the  */
   /* length of the output array. If a filter was set, the    */
   /* length of the filtered array will fix the length of the */
   /* output array.                                           */
   /*---------------------------------------------------------*/

   if (numpars > 0) {
      getcoldata( &len, numpars, cols );                /* Create and fill the dataIN array */
   } else {
      len = colsize;
   }
   dataOUT   = (float *) calloc( len, sizeof(float) );  /* Allocate space for output */
   fieresult = fiedo_c( dataIN, &len, dataOUT, &fid );  /* Fill the output array */

   if (fieresult < 0) {
      anyoutC( 1, "No evaluation possible!" );
      return;
   }
   /* Write this output to one (or more) column(s), update number of columns */
   putcol( dataOUT, len, &cols );
   *currentcols = cols;
   free( dataOUT );
   free( dataIN );
}


static void appendset( int *varcols, int *vartabs )
/*-------------------------------------------------------------*/
/* User wants the table directory of another set or wants to   */
/* extend this directory with entries from another set. In the */
/* latter case, don't reset the column counter.                */
/*-------------------------------------------------------------*/
{
   fint     r1;
   fint     nitems;
   fint     dfault;
   bool     setappend;
   int      tabs, cols;


   tabs = *vartabs;
   cols = *varcols;
   setappend = toflog( NO );
   nitems = 1;
   dfault = HIDDEN;
   Key    = tofchar("SETAPPEND=");
   Mes    = tofchar("Append new tables?    [N]");
   r1     = userlog_c( &setappend,
                    &nitems,
                    &dfault,
                    Key,
                    Mes );
   setappend = tobool( setappend );
   if (!setappend) {
      tabs = 0; cols = 0;
   }
   cancel_c(tofchar("INSET="));
   getsetname( Setin, insubs, &numsubs );
   fillcolstruc( Setin, insubs, numsubs, &cols, &tabs );
   sortandprint( cols );

   *vartabs = tabs;
   *varcols = cols;
}


static double GAMMLN( double XX )
/*-------------------------------------------------------*/
/* Returns the value LN[gamma(xx)] for xx>0              */
/*-------------------------------------------------------*/
{
   double COF[6] = { 76.18009173,
                    -86.50532033,
                     24.01409822,
                    -1.231739516,
                     0.120858003e-2,
                    -0.536382e-5 };

   double STP = 2.50662827465;
   double HALF = 0.5, ONE = 1.0 , FPF = 5.5;
   double X, TMP, SER;
   int j;


   X = XX-ONE;
   TMP = X + FPF;
   TMP = ( X + HALF ) * log(TMP) - TMP;
   SER = ONE;
   for (j = 0; j < 6; j++) {
      X = X + ONE;
      SER = SER + COF[j] / X;
   }
   return ( TMP + log( STP * SER ) );
}



static double probability( float Rlin, fint N )
/*----------------------------------------------------------------------*/
/* Calculate the probability Pc(r,N) of exceeding r in a random         */
/* sample of observations taken from an uncorrelated parent             */
/* population (rho=0).                                                  */
/* The statistic t = r * sqrt( n-2 / 1-r**2 ) is distributed in the     */
/* case of the null-hypothesis (of no correlation) like Student's t-    */
/* distribution with v = n-2 degrees of freedom. The two sided signi-   */
/* ficance level is given by 1 - A(t|v). For large n it is not          */
/* neccessary to assume a binormal distribution for the variables x, y  */
/* in the sample. r is the linear correlation coefficient for pairs of  */
/* quantities (Xi, Yi). The value of r lies between -1 and 1 inclusive. */
/* The function returns the significance level.                         */
/* If x and y are uncorrelated and we can assume that r is distributed  */
/* normally, this function returns the significance of the correlation, */
/* i.e. the probability that |r| should be larger than its observed     */
/* value in the null hypothesis. The significance level alpha is        */
/* classified as:                                                       */
/*                                                                      */
/* 5%   or less ( ttest < 0.05  )        ... significant                */
/* 1%   or less ( ttest < 0.01  )        ... highly significant         */
/* 0.1% or less ( ttest < 0.001 )        ... very highly significant    */
/*----------------------------------------------------------------------*/
{
   double freedom, free;
   double r, R2;
   int    i, imax;
   double term;
   double fi, fnum, sum, denom, pcorre;
   int    uneven;


   /*-------------------------------------------------------------------------*/
   /* Algorithm: Bevington, Philip R., 1969, Data Reduction and Error         */
   /*            Analysis for the Physical Sciences (New York: McGraw-Hill),  */
   /*            Chapter 6.                                                   */
   /*-------------------------------------------------------------------------*/

   r = (double) Rlin;
   R2 = r * r;
   freedom = (double) N - 2.0;
   if (freedom  < 0.0) return 0.0;
   if ((1.0 - R2) <= 0.0) return 0.0;
   uneven = fmod( freedom, 2.0 );
   if (!uneven) {
      imax = (int) (freedom -2.0) / 2.0;
      free = freedom;
      term = fabs( r );
      sum = term;
      if (imax < 0) return 0.0;
      if (imax == 0) return (float) (1.0 - term);
      for (i = 1; i <= imax; i++) {
         fi = (double) i;
         fnum = (double) (imax - i + 1.0);
         denom = 2.0 * i + 1;
         term = -term * R2 * fnum/fi;
         sum = sum + term/denom;
      }

      pcorre = 1.128379167 * exp( GAMMLN((free+1.0)/2.0)) /
                             exp( GAMMLN( free / 2.0 ));
      pcorre = 1.0 - pcorre * sum;
   } else {
      imax = (int) (freedom - 3) / 2;
      term = fabs( r ) * sqrt( 1.0 - R2 );
      sum = atan( R2/term );
      if (imax < 0) {
         return (float) (1.0 - 0.6366197724*sum);
      }
      if (imax == 0) {
         sum += term;
         return (float) (1.0 - 0.6366197724*sum);
      }
      sum  += term;
      for (i = 1; i <= imax; i++) {
         fnum  = 2.0 * (double) i;
         denom = 2.0 * (double) i + 1.0;
         term = term * (1.0-R2) * fnum/denom;
         sum = sum + term;
      }
      pcorre = 1.0 - 0.6366197724*sum;
   }
   if (pcorre < 0.0) pcorre = 0.0;
   return (pcorre);
}



static fint sums( float *x, float *y, fint ndata,
                  float *fsumX, float *fsumY, float *fsumXX,
                  float *fsumXY, float *fsumYY, bool *first )
/*--------------------------------------------------------*/
/* Determine different sum, used in the linear regression */
/* Returned is the number of valid data pairs, i.e.       */
/* x nor y is a blank.                                    */
/*--------------------------------------------------------*/
{
   static double  sumX, sumY, sumXX, sumYY, sumXY;
   static fint    ntot;
   double         X, Y;
   int            i;


   if (*first) {
      ntot = 0;
      sumX = sumY = 0.0;
      sumXX = sumYY = sumXY = 0.0;
   }
   for (i = 0; i < ndata; i++) {
      if ( (x[i] != Fblank) && (y[i] != Fblank) ) {
         ntot += 1;
         X = (double) x[i];
         Y = (double) y[i];
         sumX += X;
         sumY += Y;
         sumXX += X * X;
         sumXY += X * Y;
         sumYY += Y * Y;
      }
   }
   *fsumX  = (float) sumX;
   *fsumY  = (float) sumY;
   *fsumXX = (float) sumXX;
   *fsumXY = (float) sumXY;
   *fsumYY = (float) sumYY;
   *first  = 0;
   return ntot;
}




static fint linreg( fint ndata, float *fsumX, float *fsumY, float *fsumXX,
                    float *fsumXY, float *fsumYY, float *m, float *b,
                    float *corrcoeff, float *sigM, float *sigB,
                    float *chi2 )
/*-----------------------------------------------------------------------*/
/* INPUT:   fsumX etc, ndata                                             */
/* OUTPUT:  m, b, corrcoeff, sigM, sigB, corrcoeff, chi2                 */
/* PURPOSE: Input is sumX/Y/XX/XY/YY, and ndata, the number of data-     */
/*          points. In y = mx + b the parameters m and b are determined  */
/*          with a method called linear regression. Also a correlation-  */
/*          coefficient is determined. If there is not enough data, the  */
/*          values m = 0 and b = 0 are returned. The function itself     */
/*          then returns 0;                                              */
/*          The independent quantities are stored in x, the dependent    */
/*          data points are stored in y. sigM and sigB are the           */
/*          uncertainties in m and b (Bev. 6.21, 6.22).                  */
/*          In this routine there is no weighting ==>                    */
/*          chi2 = (ndata-2) * variance                                  */
/*          Bevington, Philip R., 1969, Data Reduction and Error         */
/*          Analysis for the Physical Sciences (New York: McGraw-Hill),  */
/*          Chapter 6.                                                   */
/*-----------------------------------------------------------------------*/
{
   double  sumX, sumY, sumXX, sumYY, sumXY;
   double  N;
   double  freedom;
   double  delta;
   double  variance;
   double  A, B, sigmA, sigmB;                              /* Y = A + BX !!! */
   double  Rlin;                            /* Linear correlation coefficient */


   N = (double) ndata;
   sumX  = (double) *fsumX;
   sumY  = (double) *fsumY;
   sumXX = (double) *fsumXX;
   sumXY = (double) *fsumXY;
   sumYY = (double) *fsumYY;

   if (ndata < 3) {
      anyoutC( 1, "Not enough data pairs" );
      *m = *b = 0.0;
      *sigM = *sigB = 0.0;
      return(0);
   }

   delta      = N*sumXX - sumX*sumX;
   A          = (sumXX*sumY - sumX*sumXY) / delta;
   B          = (sumXY*N    - sumX*sumY ) / delta;
   freedom    = N - 2.0;
   variance   = (sumYY + A*A*ndata + B*B*sumXX -
                 2.0*(A*sumY+B*sumXY - A*B*sumX)) / freedom;
   sigmA      = sqrt( variance*sumXX / delta );
   sigmB      = sqrt( variance*N     / delta );
   Rlin       = (N*sumXY - sumX*sumY) / sqrt(delta*(N*sumYY - sumY*sumY));
   *m         = (float) B;
   *b         = (float) A;
   *sigM      = (float) sigmB;
   *sigB      = (float) sigmA;
   *corrcoeff = (float) Rlin;
   *chi2      = (float) freedom * variance;
   return(1);
}


static void statistics( int currentcols )
/*-----------------------------------------------------------------*/
/* Do some elementary statistics on the data of a specified column */
/*-----------------------------------------------------------------*/
{
   int     kx;
   fint    sizeX;
   float   *Xarray = NULL;
   float   Xmin=0.0, Xmax=0.0;
   fint    dfault;
   float   mean, rms;
   fint    nblanks;
   fint    ntot = 0;
   int     scr = 3;
   int     v = 0;
   char    tablename[20];
   bool    tableonly;
   int     maxcols;
   int     nc;


   if (currentcols == 0) {
      anyoutC( 1, "No columns available" );
      return;
   }
   do {
      dfault  = REQUEST;
      Key     = tofchar("STABCOL=");
      Mes     = tofchar("Table, col. for statistics:        [return to menu]");
      maxcols = 1;
      nc      = gettabcols( Key, Mes, dfault, maxcols, currentcols, &kx,
                            tablename,  &tableonly );
      cancel_c( Key );
      if (nc == 0) return;
      displaytableinfo( kx );
      sizeX = (fint) fillrealarray( kx, &Xarray );
      if (filter) {
         v = filterreal( Xarray, (int) sizeX );
         displayfilter( filtercol, sizeX-v );
         sizeX = (fint) v;
      }
      if (Xarray != NULL) {
         ntot = 0;
         statr_c( Xarray, &sizeX, &Xmin, &Xmax, &mean, &rms, &nblanks, &ntot );
         anyoutC( scr, "STATISTICS");
         anyoutC( scr, "==========");
         (void) sprintf( message, "Table           : %s", column[kx].tablename );
         anyoutC( scr, message );
         (void) sprintf( message, "Column          : %s", column[kx].columnname );
         anyoutC( scr, message );
         (void) sprintf( message, "Units           : %s", column[kx].units );
         anyoutC( scr, message );
         (void) sprintf( message, "# Checked       : %d", ntot );
         anyoutC( scr, message );
         (void) sprintf( message, "Minimum         : %f", Xmin );
         anyoutC( scr, message );
         (void) sprintf( message, "Maximum         : %f", Xmax );
         anyoutC( scr, message );
         (void) sprintf( message, "Sum             : %f", mean*(ntot-nblanks) );
         anyoutC( scr, message );
         (void) sprintf( message, "Mean            : %f", mean );
         anyoutC( scr, message );
         (void) sprintf( message, "Rms             : %f", rms );
         anyoutC( scr, message );
         (void) sprintf( message, "# Blanks        : %d", nblanks );
         anyoutC( scr, message );
         free( Xarray );
         if (filter) {
            sprintf( message, "Filter length   : %d", filtersize );
            anyoutC( scr, message );
            sprintf( message, "Filtered entries: %d", sizeX-v );
            anyoutC( scr, message );
         }
      }
   } while (YES);
}




static void editcol( fint currentcols )
/*---------------------------------------------*/
/*---------------------------------------------*/
{
   fint     nitems;
   fint     dfault;
   fint     r1;
   fint     r2 = 0;
   int      i, j;
   int      k;
   fint     indx[MAXEDIT];
   float   *realarray   = NULL;
   double  *doublearray = NULL;
   fint    *fintarray   = NULL;
   bool    *logarray    = NULL;
   char    *chararray   = NULL;
   fchar    Fchararray;
   int      nchar;
   bool     realtype , dbletype , logtype , inttype , chartype;
   fint     start;
   fint     size;
   fchar    Setname;
   fchar    Tabname, Colname;
   fint     subset;
   int      left;
   char     tablename[20];
   bool     tableonly;
   int      maxcols;
   int      nc;


   if (currentcols == 0) {
      anyoutC( 1, "No columns available" );
      return;
   }
   dfault = REQUEST;
   Key    = tofchar("ETABCOL=");
   Mes    = tofchar("Table, col. to edit data:        [Return to menu]");
   maxcols = 1;
   nc      = gettabcols( Key, Mes, dfault, maxcols, currentcols, &k,
                         tablename,  &tableonly );
   cancel_c( Key );
   if (nc == 0) return;

   displaytableinfo( k );


   /* Get type of wanted array */

   realtype = (strstr(column[k].type, "REAL") != NULL);
   dbletype = (strstr(column[k].type, "DBLE") != NULL);
   inttype  = (strstr(column[k].type, "INT") != NULL);
   logtype  = (strstr(column[k].type, "LOG") != NULL);
   chartype = (strncmp( "CHAR", column[k].type, 4) == 0);


   /* Allocate memory and fill appropriate array */

   if (realtype) realarray   = column2real(k);
   if (dbletype) doublearray = column2double(k);
   if (inttype ) fintarray   = column2fint(k);
   if (logtype ) logarray    = column2log(k);
   if (chartype) chararray   = column2char( k, &nchar );

   if ( (realtype && (realarray   == NULL)) ||
        (dbletype && (doublearray == NULL)) ||
        (inttype  && (fintarray   == NULL)) ||
        (logtype  && (logarray    == NULL)) ||
        (chartype && (chararray   == NULL)) ) {
      anyoutC( 1, "Cannot allocate memory for this column!" );
      return;
   }

   /* Get the rows that have to be edited */
   size = (fint) column[k].size;
   do {
      do {
         Key     = tofchar("ROW=");
         (void) sprintf( message, "Give row(s) (1..%d) to edit:    [stop]", size);
         Mes     = tofchar(message);
         dfault  = REQUEST;
         nitems  = MAXEDIT;
         r1      = userint_c( indx, &nitems, &dfault, Key, Mes );
         agreed = YES;
         if (r1 != 0) {
            for(i = 0; i < r1; i++) {
               indx[i]--;
               agreed  = ((indx[i] >= 0) && (indx[i] < size));
               if (!agreed) break;
            }
            if (!agreed) {
               (void) sprintf( message, "Row %d outside range", indx[i]);
               reject_c( Key, tofchar(message) );
            }
         }
      } while(!agreed);
      Key    = tofchar("NEWVAL=");
      dfault = REQUEST;
      left   = r1;
      nitems = MYMIN( left, MAXEDIT );
      if (chartype) fmake(Fchararray, VARLEN);
      if (r1 != 0) {                            /* At least one entry to be edited */
         for (j = 0; j < (int) r1;) {       /* Loop over these entries (rows in a column) */
            int   rownr = indx[j];
            int   entry = rownr + 1;
            if (realtype) {
               (void) sprintf( message, "New REAL value(s) at entry %d:    [%f]",
                               entry, realarray[rownr] );
               r2 = userreal_c( &realarray[rownr], &nitems, &dfault, Key, tofchar(message) );
            }
            if (dbletype) {
               (void) sprintf( message, "New DBLE value(s) at entry %d:    [%f]",
                               entry, doublearray[rownr] );
               r2 = userdble_c( &doublearray[rownr], &nitems, &dfault, Key, tofchar(message) );
               if ((float) doublearray[rownr] == Fblank) doublearray[rownr] = Dblank;
            }
            if (inttype) {
               (void) sprintf( message, "New INT value(s) at entry %d:    [%d]",
                               entry, fintarray[rownr] );
               r2 = userint_c( &fintarray[rownr], &nitems, &dfault, Key, tofchar(message) );
            }
            if (logtype) {
               if (tobool(logarray[rownr])) {
                  (void) sprintf( message, "New LOG value(s) at entry %d:     [TRUE]", entry );
               } else {
                  (void) sprintf( message, "New LOG value(s) at entry %d:    [FALSE]", entry );
               }
               r2 = userlog_c( &logarray[rownr], &nitems, &dfault, Key, tofchar(message) );
            }
            if (realtype || dbletype || inttype || logtype) {
               if (r2 == 0) r2 = 1;         /* The default */
               left  -= r2;
               j     += r2;
               nitems = MYMIN( left, MAXEDIT );
            }
            if (chartype) {
               Fchararray.a = &chararray[rownr*nchar];
               Fchararray.l = nchar;
               (void) sprintf( message, "Text entry %d:    [%.*s]",
                               entry, nelc_c(Fchararray), Fchararray.a );
               r2 = usertext_c( Fchararray, &dfault, Key, tofchar(message) );
               j++;
            }
            cancel_c( tofchar("NEWVAL=") );
         }
      }
      cancel_c( tofchar("ROW=") );
   } while (r1 != 0);

   /* Write arrays and free allocated memory */

   fmake( Setname, NORMLEN );
   fmake( Tabname, 8 );
   fmake( Colname, 8 );
   subset  = (fint) column[k].subset;
   (void) str2char(column[k].setname, Setname);
   (void) str2char(column[k].tablename, Tabname);
   (void) str2char(column[k].columnname, Colname);
   start   = 1;
   if (realtype) {
      gdsa_wcreal_c( Setname, &subset, Tabname, Colname, realarray,   &start, &size, &r1 );
      free( realarray );
   }
   if (dbletype) {
      gdsa_wcdble_c( Setname, &subset, Tabname, Colname, doublearray, &start, &size, &r1 );
      free( doublearray );
   }
   if (inttype) {
      gdsa_wcint_c(  Setname, &subset, Tabname, Colname, fintarray,   &start, &size, &r1 );
      free( fintarray );
   }
   if (logtype) {
      gdsa_wclog_c(  Setname, &subset, Tabname, Colname, logarray,    &start, &size, &r1 );
      free( fintarray );
   }
   if (chartype) {
      Fchararray.a = chararray;
      Fchararray.l = nchar;
      gdsa_wcchar_c( Setname, &subset, Tabname, Colname, Fchararray,  &start, &size, &r1 );
      free( chararray );
   }
   if (r1 < 0) errmsg( r1 );
}




static void correlation( int currentcols )
/*-----------------------------------------------*/
/* Calculate regression and correlataion of data */
/* in two columns. Plot the regression lines in  */
/* a plot                                        */
/*-----------------------------------------------*/
{
   int      j, kx, ky;
   fint     r1;
   fint     sizeX, sizeY, size;
   floatptr Yarray = NULL;
   floatptr Xarray = NULL;
   float    Xmin=0.0, Xmax=0.0;
   float    Ymin=0.0, Ymax=0.0;
   fint     nitems;
   fint     dfault;
   char     xunits[VARLEN];
   char     yunits[VARLEN];
   float    delta;
   int      v = 0;
   float    sumX, sumY, sumXX, sumXY, sumYY;
   float    M, B, sigM, sigB, Rlin, chi2;
   float    xl, yl;
   double   prob;
   bool     first;
   char     tablename[20];
   bool     tableonly;
   int      maxcols;
   int      nc;


   if (currentcols == 0) {
      anyoutC( 1, "No columns available" );
      return;
   }
   do {
      dfault  = REQUEST;
      Key     = tofchar("YTABCOL=");
      Mes     = tofchar("Table, col. for Y values:        [return to menu]");
      maxcols = 1;
      nc      = gettabcols( Key, Mes, dfault, maxcols, currentcols, &ky,
                            tablename,  &tableonly );
      cancel_c( Key );
      if (nc == 0) {
         return;
      } else {
         sizeY = (fint) fillrealarray( ky, &Yarray );
         if (Yarray != NULL) {
            (void) strcpy( yunits, column[ky].units );
            minmax1_c( Yarray, &sizeY, &Ymin, &Ymax );
            dfault  = REQUEST;
            Key     = tofchar("XTABCOL=");
            Mes     = tofchar("Table, col. for X values:  [Create X column]");
            maxcols = 1;
            nc      = gettabcols( Key, Mes, dfault, maxcols, currentcols, &kx,
                                  tablename,  &tableonly );
            cancel_c( Key );
            if (nc == 0) {
               /*-------------------------------------*/
               /* User wants to fill his own X array. */
               /* Do not exceed size of Y array.      */
               /*-------------------------------------*/
               int     left = 0;
               sizeX = sizeY;
               Xarray = (float *) calloc( (int) sizeX, sizeof(float) );
               if (Xarray == NULL) {
                  anyoutC( 1, "Cannot allocate memory to read column" );
               }
               Xmin = 1.0; Xmax = (float) sizeX;
               nitems = sizeX;
               dfault = REQUEST;
               Xmin   = 1.0;
               Xmax   = (float) sizeX;
               Key    = tofchar("XVALUES=");
               (void) sprintf( message, "Give %d values for X axis:      [calculated]", nitems );
               Mes    = tofchar( message );
               r1     = userreal_c( Xarray, &nitems, &dfault, Key, Mes );
               left   = nitems - r1;
               cancel_c( Key );
               if (r1 == 0) {
                  delta  = (Xmax - Xmin) / ((float)sizeX - 1);
                  for (j = 0; j < (int) sizeX; j++) {
                     Xarray[j] = Xmin + j*delta;
                  }
               } else {
                  while(left != 0) {
                     (void) sprintf( message, "Still need %d values: ", left );
                     Mes   = tofchar( message );
                     r1    = userreal_c( &Xarray[nitems-left], &nitems, &dfault, Key, Mes );
                     left -= r1;
                     cancel_c( Key );
                  }
               }
               (void) strcpy( xunits, "X" );
            } else {
               /* Get the x-values */
               (void) strcpy( xunits, column[kx].units );
               sizeX = (fint) fillrealarray( kx, &Xarray );
               r1 = 0;
            }
            if (Xarray != NULL) {
               minmax1_c( Xarray, &sizeX, &Xmin, &Xmax );
               /* Plot contents of array(s) and take care of blanks */
               size = MYMIN( sizeX, sizeY );
               (void) sprintf( message, "TABLE" );
               if ( (Xmin == Fblank) || (Xmax == Fblank) || (Ymin == Fblank) || (Ymax == Fblank) ) {
                  anyoutC( 1, "Cannot draw frame (min or max is blank)" );
               } else {
                  int    f;
                  fint   g = 0;
                  fchar  Ttitle;

                  dfault = 1;
                  Key = tofchar("HEADER=");
                  Mes = tofchar("Give text as header above plot:" );
                  Ttitle.a = message;
                  Ttitle.l = 80;
                  r1 = usertext_c( Ttitle, &dfault, Key, Mes );
                  message[r1] = '\0';
                  drawframe( &Xmin, &Ymin, &Xmax, &Ymax, xunits, yunits, message );
                  for (f = 0; f < (int) size; f++) {
                     if ( (Xarray[f] != Fblank) && (Yarray[f] != Fblank) ) {
                        Xarray[g] = Xarray[f]; Yarray[g] = Yarray[f];
                        g++;
                     } else {
                        pgpt_c( &g, Xarray, Yarray, &symbol );
                        g = 0;
                     }
                  }
                  if (g != 0) {   /* Some or all left? */
                     pgpt_c( &g, Xarray, Yarray, &symbol );
                  }
                  first = YES;


                  /* Is a filter used? */

                  if (filter) {
                     v = filterreal( Xarray, (int) size );
                     v = filterreal( Yarray, (int) size );
                     displayfilter( filtercol, size-v );
                     size = (fint) v;
                  }

                  r1 = sums( Xarray, Yarray, (fint) size,
                             &sumX, &sumY, &sumXX, &sumXY,&sumYY, &first );

                  /* Display results */

                  anyoutC( 3," ");
                  anyoutC( 3,"CORRELATION AND REGRESSION" );
                  anyoutC( 3,"==========================" );
                  (void) sprintf( message, "Data points diagram  : n = %d", r1 );
                  anyoutC( 3, message );
                  linreg( r1, &sumX, &sumY, &sumXX, &sumXY,&sumYY,
                          &M, &B, &Rlin, &sigM, &sigB, &chi2 );
                  (void) sprintf( message, "Regression Y on X    : Y = %+f (+/-%f) X %+f (+/-%f)",
                                  M, fabs(sigM), B, fabs(sigB) );
                  anyoutC( 3, message );
                  /* Draw this line */
                  xl = Xmin; yl = M*Xmin + B; pgmove_c( &xl, &yl );
                  xl = Xmax; yl = M*Xmax + B; pgdraw_c( &xl, &yl );

                  linreg( r1, &sumY, &sumX, &sumYY, &sumXY, &sumXX,
                          &M, &B, &Rlin, &sigM, &sigB, &chi2 );

                  (void) sprintf( message, "Regression X on Y    : X = %+f (+/-%f) Y %+f (+/-%f)",
                                  M, fabs(sigM), B, fabs(sigB) );
                  anyoutC( 3, message );
                  yl = Ymin; xl = M * Ymin + B;  pgmove_c( &xl, &yl );
                  yl = Ymax; xl = M * Ymax + B;  pgdraw_c( &xl, &yl );

                  (void) sprintf( message, "Linear corr. coeff.  : r = %f", Rlin );
                  anyoutC( 3, message );

                  prob = 100.0 * probability( Rlin, r1);
                  anyoutC( 3, "Probability that     ");
                  anyoutC( 3, "Parent Distribution  ");
                  (void) sprintf( message, "is uncorrelated      : Pc(r,n) = %#g(%)", prob );
                  if (prob >= 5.0)      (void) strcat( message, " ( Not significant )" );
                  else if (prob <= 5.0) (void) strcat( message, " ( Significant )" );
                  else if (prob <= 1.0) (void) strcat( message, " ( Highly significant )" );
                  else if (prob <= 0.1) (void) strcat( message, " ( Very highly significant )" );
                  anyoutC( 3, message );
               }
               free( Xarray );
            }
            free( Yarray );
         }
      }
   } while (YES);
}


static bool setfiltercolumn( int currentcols )
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
{
   fint     dfault;
   bool     agreed;
   bool     realtype , dbletype , logtype , inttype , chartype;
   int      k;
   float   *realarray   = NULL;
   double  *doublearray = NULL;
   fint    *fintarray   = NULL;
   int      w;
   char     tablename[20];
   bool     tableonly;
   int      maxcols;
   int      nc;



   if (currentcols == 0) {
      anyoutC( 1, "No columns available" );
      return( NO );
   }
   realtype = dbletype = logtype = inttype = chartype = NO;
   do {
      dfault  = REQUEST;
      Key     = tofchar("FTABCOL=");
      Mes     = tofchar("Table, col. number for filter:        [Filter OFF]");
      maxcols = 1;
      nc      = gettabcols( Key, Mes, dfault, maxcols, currentcols, &k,
                            tablename,  &tableonly );
      if (nc == 0) {
         cancel_c( Key );
         anyoutC( 3, "====> FILTER IS OFF" );
         return(NO);
      }
      realtype = (strstr(column[k].type, "REAL") != NULL);
      dbletype = (strstr(column[k].type, "DBLE") != NULL);
      inttype  = (strstr(column[k].type, "INT" ) != NULL);
      logtype  = (strstr(column[k].type, "LOG" ) != NULL);
      chartype = (strncmp( "CHAR", column[k].type, 4) == 0);
      agreed = (realtype || dbletype || inttype || logtype);
      if (!agreed) {
         reject_c( Key, tofchar("Cannot convert") );
         anyoutC( 1, "Cannot convert this data to type LOG" );
      }
   } while (!agreed);
   if (filterarray != NULL) free(filterarray);
   cancel_c( Key );
   filtersize  = column[k].size;
   filterarray = (bool *) calloc( filtersize, sizeof(bool) );
   filtercol   = k;

   /* Allocate memory and fill appropriate array, convert to bool */

   if (realtype) {
      realarray   = column2real(k);
      for (w = 0; w < filtersize; w++) {
         if (realarray[w] == Fblank) {
            filterarray[w] = NO;
         } else {
            filterarray[w] = toflog(realarray[w]);
         }
      }
      free(realarray);
   }
   if (dbletype) {
      doublearray = column2double(k);
      for (w = 0; w < filtersize; w++)
      if (doublearray[w] == Dblank) {
         filterarray[w] = NO;
      } else {
         filterarray[w] = toflog(doublearray[w]);
      }
      free(doublearray);
   }
   if (inttype ) {
      fintarray   = column2fint(k);
      for (w = 0; w < filtersize; w++) filterarray[w] = toflog(fintarray[w]);
      free(fintarray);
   }
   if (logtype ) filterarray = column2log(k);
   anyoutC( 3, "====> FILTER IS ON" );
   displaytableinfo( filtercol );
   return(YES);
}



static void plotid( void )
/*------------------------------------------------------------*/
/* PURPOSE: Plot user id. and date at user supplied position. */
/* Ask position (in mm) of id. The id. string is plotted at   */
/* that position. The default string is date, user.           */
/*------------------------------------------------------------*/
{
   fint    nitems = 1;
   fint    dfault = REQUEST;
   fint    r;
   float   pos[2];
   float   angle = 0.0;
   float   jus = 0.0;
   fchar   Idstr;
   fchar   Key, Mes;
   char    mes1[LONGLEN];
   char    mes2[60];


   fmake( Key, KEYLEN );
   fmake( Mes, NORMLEN );
   pos[0] = pos[1] = 3.0;
   Key    = tofchar("IDPOS=");
   Mes.l  = sprintf( Mes.a, "Enter position of id. in mm:   [%g %g]",
                     pos[0], pos[1] );
   r      = userreal_c( pos, &nitems, &dfault, Key, Mes );
   cancel_c( Key );

   Key    = tofchar("IDANGLE=");
   Mes.l  = sprintf( Mes.a, "Enter angle of id. string (deg):  [%g]", angle );
   r      = userreal_c( &angle, &nitems, &dfault, Key, Mes );
   cancel_c( Key );


   fmake( Idstr, LONGLEN );
   Key    = tofchar("IDTXT=");
   Mes.l  = sprintf( Mes.a, "Enter text for id.:   [user,date]" );
   r      = usertext_c( Idstr, &dfault, Key, Mes );
   cancel_c( Key );

   if (!r)
   {
      getusernam_c( Idstr );
      sprintf( mes1, "\\fi GIPSY:\\fn %.*s, ",
               nelc_c( Idstr ), Idstr.a );
      getdate_c( Idstr );
      sprintf( mes2, "%.*s", nelc_c( Idstr ), Idstr.a );
      strcat( mes1, mes2 );
      Idstr = tofchar( mes1 );
   }
   {
      /* Set window+viewport to mm */
      fint   mm = 2;
      float  nx1 = 0.0, nx2 = 1.0, ny1 = 0.0, ny2 = 1.0;
      float  x1, x2, y1, y2;

      pgsvp_c( &nx1, &nx2, &ny1, &ny2 );
      pgqvp_c( &mm, &x1, &x2, &y1, &y2 );
      pgswin_c( &x1, &x2, &y1, &y2 );
   }
   pgptxt_c( &pos[0], &pos[1], &angle, &jus, Idstr );
}




MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   int     cols, tabs;
   int     option;
   int     exit;


   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      (void) myname_c( Task );             /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &Fblank );
   setdblank_c( &Dblank );
   fmake( Setin, NORMLEN );
   fmake( Key, KEYLEN );
   fmake( Mes, NORMLEN );
   /* Get set name and display first time table directory */
   tabs = 0; cols = 0;
   getsetname( Setin, insubs, &numsubs );
   fillcolstruc( Setin, insubs, numsubs, &cols, &tabs );
   sortandprint( cols );
   do {
      option = getchoice();
      exit   = NO;
      switch (option) {
         case 1 : exit = YES;                       /* Quit Hermes */
                  break;
         case 2 : appendset( &cols, &tabs );        /* New list or append to old */
                  break;
         case 3 : filter = setfiltercolumn( cols ); /* Set up a filter */
                  break;
         case 4 : importdata( &cols );              /* Create col. and import data */
                  break;
         case 5 : displaycol( cols );               /* Format output of columns */
                  break;
         case 6 : if (opendisplay) pgend_c();       /* Plot column data */
                  initplot();
                  opendisplay = YES;
                  plotcols( cols );
                  break;
         case 8 : calc( &cols );                    /* Calculate with column data */
                  break;
         case 9 : statistics( cols );               /* Elementary statistics */
                  break;
         case 10: if (opendisplay) pgend_c();       /* Correlation between two columns */
                  initplot();
                  opendisplay = YES;
                  correlation( cols );
                  break;
         case 11: /* Sorting */
                  anyoutC( 1, "Not yet implemented" );
                  break;
         case 12: /* edit data */                   /* Edit entries in a column */
                  editcol( cols );
                  break;
         case 13: delcoltab( cols );                /* Delete table/column(s) */
                  break;
         case 14: /* Copying */                     /* Copy (or append) */
                  copycol( &cols );
                  break;
      }
   } while (!exit);
   if (opendisplay)
   {
      plotid();
      pgend_c();
   }
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
